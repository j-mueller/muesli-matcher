{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Muesli matcher node client
-}
module Convex.Muesli.NodeClient(
  muesliClient,
  -- * Debug
  applyTx,
  ProcessingEnv(..),
  MuesliState(..),
  PrintMode(..),
  printState,
  potentialOrders,
  initialState,
  applyNewOutputEvent,
  txInLocations,
  insertMatch,
  isOwnTxOut
  ) where

import Cardano.Api (AlonzoEra, AssetId (..), Block (..), BlockInMode (..), CardanoMode, Env, Quantity (..), SlotNo,
                    Tx (..), TxIn (..))
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Control.Lens (anon, at, makeLenses, use, (%=), (&), (.=), (.~), (<>=), (?=))
import Control.Monad (join, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State.Strict (execStateT)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Writer.Strict (execWriterT)
import Convex.Event (NewOutputEvent (..), OutputSpentEvent (..), ResolvedInputs (..), TxWithEvents (..), extract,
                     splitEvent, txIn)
import Convex.Muesli.Constants (MuesliVersion)
import Convex.Muesli.Constants qualified as Constants
import Convex.Muesli.Keys qualified as Keys
import Convex.Muesli.KnownOrder (KnownOrder (..))
import Convex.Muesli.KnownOrder qualified as KnownOrder
import Convex.Muesli.Match (Match (..), Valid)
import Convex.Muesli.Match qualified as Match
import Convex.Muesli.MatchWorker (WorkerEnv (..))
import Convex.Muesli.MatchWorker qualified as MatchWorker
import Convex.Muesli.Stats (MuesliStats, Stats)
import Convex.Muesli.Stats qualified as Stats
import Convex.NodeClient (PipelinedLedgerStateClient)
import Convex.NodeClient.Fold (CatchingUp (..), foldClient)
import Data.Either (partitionEithers)
import Data.Foldable (fold, toList, traverse_)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (sortOn)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Validation (Validation (..))
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)

addMatches :: [Valid Match] -> WorkerEnv -> IO ()
addMatches matches WorkerEnv{weMatches} =
  let sorted =
        fmap MatchWorker.initMatchWithTxAttempts
        $ reverse
        $ sortOn (C.selectLovelace . Match.matchValue . Match.getValid)
        $ take 11
        $ reverse
        $ sortOn (Match.firstOrderPlaced . Match.getValid) matches
  in STM.atomically $ STM.modifyTVar weMatches (sorted Seq.<|)

pruneQueue :: WorkerEnv -> IO ()
pruneQueue WorkerEnv{weMatches} = STM.atomically $ STM.modifyTVar weMatches (Seq.take 15)

flushStats :: WorkerEnv -> IO Stats
flushStats WorkerEnv{weStats} = STM.atomically $ do
  r <- STM.readTVar weStats
  STM.writeTVar weStats mempty
  pure r

{-| Whether we can print something to the console
-}
data PrintMode =
  Print String -- ^ Yes it's ok to print, use this prefix
  | DontPrint -- ^ No, don't print anything
  deriving (Eq, Ord, Show)

shouldPrint :: BlockInMode CardanoMode -> CatchingUp -> PrintMode
shouldPrint currentBlock catchingUp =
  let (C.BlockInMode (C.Block (C.BlockHeader _slotNo _blockHash (C.BlockNo blockNo)) _) _) = currentBlock in
  if (blockNo `mod` 10_000 == 0 || catchingUp == CaughtUpWithNode)
    then Print (show blockNo <> ": ")
    else DontPrint

data ProcessingEnv =
  ProcessingEnv
    { printMode   :: PrintMode
    , catchingUp  :: CatchingUp
    , currentSlot :: SlotNo
    , workerEnv   :: Maybe WorkerEnv
    }

data MuesliState =
  MuesliState
    { _resolvedInputs  :: !(ResolvedInputs MuesliVersion)
    , _potentialOrders :: !(Map AssetId (IntMap (Map AssetId (IntMap (Map TxIn (KnownOrder, NewOutputEvent MuesliVersion)))))) -- orders indexed by bid and ask
    , _txInLocations   :: !(Map TxIn ((AssetId, Quantity), (AssetId, Quantity)))
    , _muesliStats     :: !MuesliStats
    , _allTimeStats    :: !Stats.Stats
    } deriving Show

makeLenses ''MuesliState

totalOpenOffers :: MuesliState -> Int
totalOpenOffers = Map.size . _txInLocations

initialState :: MuesliState
initialState =
  MuesliState
    { _resolvedInputs = mempty
    , _txInLocations = mempty
    , _potentialOrders = mempty
    , _muesliStats = Stats.emptyStats
    , _allTimeStats = mempty
    }

muesliClient ::
  TVar Stats
  -> TVar MuesliStats
  -> Maybe WorkerEnv
  -> Env
  -> PipelinedLedgerStateClient
muesliClient allStatsTVar muesliStatsTVar workerEnv env = foldClient initialState env $ \catchingUp currentState currentBlock -> do
  let printMode = shouldPrint currentBlock catchingUp
      BlockInMode (Block blockHeader _) _ = currentBlock
      C.BlockHeader currentSlot _ _ = blockHeader
      (newEvents, newInputs) = extract Constants.muesliVersion (_resolvedInputs currentState) currentBlock
      newState' = currentState & resolvedInputs .~ newInputs
      eventStats = Stats.countEvents (length newEvents)

  runMaybeT
    $ flip runReaderT ProcessingEnv{workerEnv, printMode, catchingUp, currentSlot}
    $ do
      flip execStateT newState' $ do
        (processingStats :: Stats.Stats) <- execWriterT @_ @Stats.Stats $ do
          potentialMatches <- join <$> traverse applyTx newEvents
          flip traverse_ potentialMatches $ \(order, event, matches) -> do
            insertMatch order event
            when (catchingUp == CaughtUpWithNode) $ do
              validMatches <- catMaybes <$> traverse validate matches
              liftIO (traverse_ (addMatches validMatches) workerEnv)
        workerStats <- fromMaybe mempty <$> (traverse (liftIO . flushStats)) workerEnv
        let combinedStats = eventStats <> processingStats <> workerStats
        muesliStats %= Stats.prepend (Stats.fromBlockHeader blockHeader combinedStats)
        allTimeStats <>= combinedStats
        muesliStats %= fst . Stats.lastWeek
        writeTVars allStatsTVar muesliStatsTVar
        traverse_ (liftIO . pruneQueue) workerEnv
        printState

writeTVars :: (MonadState MuesliState m, MonadIO m) => TVar Stats -> TVar MuesliStats -> m ()
writeTVars allStatsTVar muesliStatsTVar = do
  MuesliState{_muesliStats, _allTimeStats} <- get
  liftIO $ STM.atomically $ do
    STM.writeTVar muesliStatsTVar _muesliStats
    STM.writeTVar allStatsTVar _allTimeStats

handleTx :: (MonadIO m, MonadWriter Stats m) => SlotNo -> Tx AlonzoEra -> m ()
handleTx fulfilmentTime (Tx txBody _keyWitnesses) = do
  let C.TxBody C.TxBodyContent{C.txOuts} = txBody
  when (any isOwnTxOut txOuts) $ do
    tell (Stats.countOrderFulfilledSelf fulfilmentTime)
    tell (Stats.countOrderFulfilledSelf fulfilmentTime)

isOwnTxOut :: C.TxOut C.CtxTx AlonzoEra -> Bool
isOwnTxOut (C.TxOut (C.AddressInEra _ (C.ShelleyAddress _ _ cred)) _ _) = cred `Set.member` Keys.ownStakingKeys
isOwnTxOut _                                                            = False

printState :: (MonadIO m, MonadState MuesliState m, MonadReader ProcessingEnv m) => m ()
printState = do
  currentState <- get
  let openOffers = "Open offers: " <> show (totalOpenOffers currentState)
      stats =
        [ "Last hour: "
        , Text.unpack (Stats.prettyStats $ Stats.stats $ fst $ Stats.lastHour $ _muesliStats currentState)
        , " "
        , "All time: "
        , Text.unpack (Stats.prettyStats $ _allTimeStats currentState)
        ]
  print_ $ unlines (openOffers : stats)

quantityInt :: Quantity -> Int
quantityInt (Quantity q) = fromIntegral q

deleteTxIn :: MonadState MuesliState m => TxIn -> m ()
deleteTxIn txi = do
  loc <- use (txInLocations . at txi)
  flip traverse_ loc $ \((bidC, bidQ), (askC, askQ)) -> do
    potentialOrders
      . at bidC . anon IntMap.empty IntMap.null
      . at (quantityInt bidQ) . anon Map.empty Map.null
      . at askC . anon IntMap.empty IntMap.null
      . at (quantityInt askQ) . anon Map.empty Map.null
      . at txi
      .= Nothing
    txInLocations . at txi .= Nothing

applyTx :: (MonadIO m, MonadState MuesliState m, MonadWriter Stats m) => TxWithEvents MuesliVersion -> m [(KnownOrder, NewOutputEvent MuesliVersion, [Match])]
applyTx TxWithEvents{twEvents, twSlot, twTx} = do
  let (spent, produced) = partitionEithers $ fmap splitEvent $ toList twEvents
  case spent of
    [outputSpent1, outputSpent2] -> do
      traverse_ (deleteTxIn . oseTxIn) spent
      let created = neSlot . oseTxOutput
          fulfilmentTime = twSlot - max (created outputSpent1) (created outputSpent2)
      tell (Stats.countOrderFulfilledTotal fulfilmentTime)
      tell (Stats.countOrderFulfilledTotal fulfilmentTime)
      handleTx fulfilmentTime twTx
    [_] -> tell Stats.countOrderCancelled
    _ -> pure ()
  catMaybes <$> traverse applyNewOutputEvent produced

applyNewOutputEvent :: (MonadIO m, MonadState MuesliState m, MonadWriter Stats m) => NewOutputEvent MuesliVersion -> m (Maybe (KnownOrder, NewOutputEvent MuesliVersion, [Match]))
applyNewOutputEvent orderOneEvent@NewOutputEvent{neTxMetadata} = do
  let offerValue = Match.getOfferValue orderOneEvent
  case KnownOrder.knownOrderFromMetadata neTxMetadata of
    Left _ -> do
      tell Stats.countMetadataFailed
      return Nothing
    Right orderOne@KnownOrder{orderAsk=(askC, askQ)} -> do
      potentialMatches <- fmap join <$> flip traverse (C.valueToList offerValue) $ \(bidC, bidQ) -> do
        offersForCurrency <- use (potentialOrders . at askC . anon IntMap.empty IntMap.null)
        let (_, potentialOffers) = IntMap.split (pred $ quantityInt askQ) offersForCurrency
            k = fold (mapMaybe (\(_, otherCurrencies) -> Map.lookup bidC otherCurrencies) (IntMap.toList potentialOffers))
            (potentialOffers', _) = IntMap.split (succ $ quantityInt bidQ) k
            mx = foldMap (Map.toList . snd) (IntMap.toAscList potentialOffers')
        flip traverse mx $ \(_, (orderTwo, orderTwoEvent)) -> do
          return Match{orderOne, orderOneEvent, orderTwo, orderTwoEvent}
      let numMatches = length potentialMatches
      tell (Stats.countTotalMatches numMatches)
      return (Just (orderOne, orderOneEvent, potentialMatches))

insertMatch :: (MonadState MuesliState m) => KnownOrder -> NewOutputEvent MuesliVersion -> m ()
insertMatch orderOne@KnownOrder{orderAsk=(askC, askQ)} orderOneEvent = do
  let offerValue = Match.getOfferValue orderOneEvent
      txi = txIn orderOneEvent
  flip traverse_ (C.valueToList offerValue) $ \(bidC, bidQ) -> do
    potentialOrders
      . at bidC . anon IntMap.empty IntMap.null
      . at (quantityInt bidQ) . anon Map.empty Map.null
      . at askC . anon IntMap.empty IntMap.null
      . at (quantityInt askQ) . anon Map.empty Map.null
      . at txi ?= (orderOne, orderOneEvent)
    txInLocations . at txi .= Just ((bidC, bidQ), (askC, askQ))

{-| Print something to the console if we are fully caught up, or once every 10 000 blocks.
-}
print_ :: (MonadIO m, MonadReader ProcessingEnv m) => String -> m ()
print_ msg = do
  ProcessingEnv{printMode} <- ask
  case printMode of
    Print prefix -> liftIO $ putStrLn $ prefix <> msg
    _            -> pure ()

validate :: MonadWriter Stats m => Match -> m (Maybe (Valid Match))
validate match = case Match.validateMatch match of
  Failure{} -> do
    tell Stats.countInvalidMatch
    pure Nothing
  Success validMatch -> do
    tell Stats.countValidMatch
    pure (Just validMatch)

data HandleMatchError =
  TxBodyErr C.TxBodyError
  | DeserialisationError
  | NodeQueryFailure AcquireFailure
  | NodeQueryFailed EraMismatch
  | NodeQueryNoConnectionInfo
  | NotSubmittedStillCatchingUp
  | NotSubmittedNoWalletEnv
  | OrderExtractionFailed KnownOrder.FromMetadataError
  deriving Show

{- Default TTL for match transactions. Should not be too long so that the wallet can re-use the outputs
-}
-- _MATCH_TX_TTL :: SlotNo
-- _MATCH_TX_TTL = 300 -- 5 minutes. With approx. 1 block per 20 slots this gives us a little more than 6 blocks.
