{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-| Worker threads that process valid matches
-}
module Convex.Muesli.MatchWorker(
  MatchWithTxAttempts(..),
  initMatchWithTxAttempts,

  initialiseWorkerEnv,
  WorkerEnv(..),
  handleMatch,
  startHandlingMatches
  ) where

import Cardano.Api.Shelley (CardanoMode, LocalNodeConnectInfo, Tx, TxId)
import Cardano.Api.Shelley qualified as C
import Cardano.Wallet.Api.Client qualified as WalletClient
import Cardano.Wallet.Api.Types qualified as WalletClient
import Cardano.Wallet.Primitive.Types (WalletId)
import Cardano.Wallet.Primitive.Types.Tx qualified as WalletClient
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, TVar)
import Control.Concurrent.STM qualified as STM
import Control.Monad (forever, unless, void, when)
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Writer.Strict (execWriterT)
import Convex.Muesli.Blockfrost qualified as Blockfrost
import Convex.Muesli.Config (CardanoWalletEnv (..))
import Convex.Muesli.ExportTx (ExportTx (..))
import Convex.Muesli.ExportTx qualified as ExportTx
import Convex.Muesli.Match (Match, Valid (..))
import Convex.Muesli.Match qualified as Match
import Convex.Muesli.Stats (Stats)
import Convex.Muesli.Stats qualified as Stats
import Data.Aeson (ToJSON (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Text qualified as Text
import Data.Text.Class (toText)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Servant.Client (ClientError, runClientM)

data MatchWithTxAttempts =
  MatchWithTxAttempts
    { mwtMatch            :: Valid Match
    , mwtAttemptedWallets :: Set WalletId
    }

initMatchWithTxAttempts :: Valid Match -> MatchWithTxAttempts
initMatchWithTxAttempts mwtMatch =
  MatchWithTxAttempts{mwtMatch, mwtAttemptedWallets = mempty}

initialiseWorkerEnv :: IO WorkerEnv
initialiseWorkerEnv =
  WorkerEnv
    <$> STM.newTVarIO mempty
    <*> STM.newTVarIO mempty

data WorkerEnv =
  WorkerEnv
    { weMatches :: TVar (Seq [MatchWithTxAttempts])
    , weStats   :: TVar Stats
    }

dequeue :: TVar (Seq a) -> STM a
dequeue tv = do
  sq <- STM.readTVar tv
  case Seq.viewl sq of
    Seq.EmptyL -> STM.retry
    a Seq.:< rest -> do
      STM.writeTVar tv rest
      return a

enqueue :: TVar (Seq a) -> a -> STM ()
enqueue tv a = STM.modifyTVar tv (Seq.|> a)

{-| Start a worker thread that submits matches to the wallet
-}
startHandlingMatches ::
  LocalNodeConnectInfo CardanoMode ->
  WorkerEnv ->
  CardanoWalletEnv ->
  IO ()
startHandlingMatches connectionInfo we@WorkerEnv{weStats} walletEnv = do
  let CardanoWalletEnv{cweWalletId} = walletEnv
  putStrLn $ "Starting worker for wallet " <> Text.unpack (toText cweWalletId)
  void $ forkIO $ forever $ do
    newStats <- execWriterT (handleMatch connectionInfo we walletEnv)
    STM.atomically (STM.modifyTVar weStats (<> newStats))

{-| Take a match from the queue and send it to the wallet
-}
handleMatch ::
  (MonadIO m, MonadWriter Stats m) =>
  LocalNodeConnectInfo CardanoMode ->
  WorkerEnv ->
  CardanoWalletEnv ->
  m ()
handleMatch connectionInfo WorkerEnv{weMatches} walletEnv = flip runReaderT connectionInfo $ do
  let hasTried MatchWithTxAttempts{mwtAttemptedWallets} = Set.member (cweWalletId walletEnv) mwtAttemptedWallets
      enq r = unless (null r) (liftIO $ STM.atomically (enqueue weMatches r))
  matchGroup <- liftIO (STM.atomically (dequeue weMatches))
  case matchGroup of
    []     -> pure ()
    (firstMatch:rest) | not (hasTried firstMatch) -> do
      let MatchWithTxAttempts{mwtMatch, mwtAttemptedWallets=wallets} = firstMatch
      result <- runExceptT (sendMatchTxn walletEnv mwtMatch)
      case result of
        Left (WalletErr BalanceTxFailed{}) -> do
          tell Stats.countBalanceTxFailed
          enq (MatchWithTxAttempts{mwtMatch, mwtAttemptedWallets = Set.insert (cweWalletId walletEnv) wallets } : rest)
        Left (WalletErr PostTxFailed{}) -> do
          tell Stats.countInvalidMatchTxnSubmitted
          enq rest
        Left (WalletErr (NotSubmittedNoProfit{})) -> do
          tell Stats.countTxnNotProfitable
          enq rest
        Left{} -> do
          tell Stats.countOtherError -- TODO: ?
          enq rest
        Right{} -> do
          liftIO $ putStrLn $ Text.unpack (toText $ cweWalletId walletEnv) <> " submit"
          tell Stats.countValidMatchTxnSubmitted
    rest -> liftIO $ STM.atomically (enqueue weMatches rest)
  pure ()

data WalletFlowError =
  BalanceTxFailed ClientError
  | SignTxFailed ClientError
  | PostTxFailed ClientError
  | NotSubmittedNoProfit{txnFee::C.Lovelace, matchMakingFee::C.Lovelace} -- ^ matchmakingFee must be higher than txnFee
  | TxWrongEra C.AnyCardanoEra
  deriving Show

data ProcessMatchError =
  TxBodyErr C.TxBodyError
  | NodeQueryFailure AcquireFailure
  | NodeQueryFailed EraMismatch
  | WalletErr WalletFlowError
  deriving Show

sendMatchTxn :: (MonadIO m, MonadReader (LocalNodeConnectInfo CardanoMode) m, MonadError ProcessMatchError m, MonadWriter Stats m) => CardanoWalletEnv -> Valid Match -> m TxId
sendMatchTxn currentWallet Valid{getValid=m} = do
  txn <- prepareMatchTxn m
  runExceptT (processMatchTxn m txn currentWallet) >>= either (throwError . WalletErr) pure

prepareMatchTxn :: (MonadIO m, MonadReader (LocalNodeConnectInfo CardanoMode) m, MonadError ProcessMatchError m) => Match -> m ExportTx
prepareMatchTxn m = do
  connectionInfo <- ask
  currentParams <- liftIO (C.queryNodeLocalState connectionInfo Nothing (C.QueryInEra C.AlonzoEraInCardanoMode (C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo C.QueryProtocolParameters)))
  case currentParams of
    Left err -> throwError (NodeQueryFailure err)
    Right (Left err) -> throwError (NodeQueryFailed err)
    Right (Right pp) ->
      runExceptT (ExportTx.mkExportTx pp m) >>= either (throwError . TxBodyErr) pure

processMatchTxn :: (MonadIO m, MonadError WalletFlowError m, MonadWriter Stats m) => Match -> ExportTx -> CardanoWalletEnv ->  m TxId
processMatchTxn match exportTx CardanoWalletEnv{cweClient, cweWalletId, cwePassphrase} = do
  WalletClient.ApiSerialisedTransaction sealedTx <- liftIO (runClientM (WalletClient.balanceTransaction WalletClient.transactionClient (WalletClient.ApiT cweWalletId) (toJSON exportTx)) cweClient) >>= liftEither . first BalanceTxFailed

  let txn = WalletClient.cardanoTx (WalletClient.getApiT sealedTx)
  checkTxnProfitable match txn

  let postData = WalletClient.ApiSignTransactionPostData sealedTx (WalletClient.ApiT cwePassphrase)
  WalletClient.ApiSerialisedTransaction (WalletClient.ApiT signedTx) <- liftIO (runClientM (WalletClient.signTransaction WalletClient.transactionClient (WalletClient.ApiT cweWalletId) postData) cweClient) >>= liftEither . first SignTxFailed

  WalletClient.ApiTxId (WalletClient.ApiT i) <- liftIO (runClientM (WalletClient.postExternalTransaction WalletClient.transactionClient (WalletClient.ApiBytesT (WalletClient.SerialisedTx $ WalletClient.serialisedTx signedTx))) cweClient) >>= liftEither . first PostTxFailed

  tell (Stats.countTxnPosted cweWalletId)

  liftIO (Blockfrost.submitTxn $ WalletClient.serialisedTx signedTx) >>= \case
    Left{}  -> tell Stats.countTxnBlockfrostFailed
    Right{} -> tell Stats.countTxnBlockfrost
  return (fromString . Text.unpack $ toText i)

checkTxnProfitable :: (MonadError WalletFlowError m) => Match -> C.InAnyCardanoEra Tx -> m ()
checkTxnProfitable match = \case
  C.InAnyCardanoEra C.AlonzoEra (C.Tx (C.TxBody C.TxBodyContent{C.txFee=C.TxFeeExplicit _ txnFee}) _) -> do
    let matchMakingFee = C.selectLovelace (Match.matchValue match)
        minProfit      = 0 -- 0 Ada 200_000 -- 0.2 Ada
    when (txnFee >= matchMakingFee - minProfit)
      $ throwError $ NotSubmittedNoProfit{txnFee,matchMakingFee}
  C.InAnyCardanoEra otherEra _ -> throwError (TxWrongEra $ C.anyCardanoEra otherEra)
