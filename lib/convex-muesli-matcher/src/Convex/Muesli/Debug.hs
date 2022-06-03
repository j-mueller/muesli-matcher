{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
module Convex.Muesli.Debug(
  debug
  ) where

import Control.Lens
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State.Strict (execStateT)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Writer.Strict (runWriterT)
import Convex.Event (NewOutputEvent (..), TxWithEvents)
import Convex.Muesli.Constants
import Convex.Muesli.KnownOrder (validateKnownOrder)
import Convex.Muesli.NodeClient
import Convex.Muesli.Stats (Stats)
import Convex.NodeClient.Fold (CatchingUp (..))
import Data.Foldable (traverse_)
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Data.Validation (Validation (..))

-- 2, 0, 1

debug :: IO (Maybe MuesliState, Stats)

debug = runWriterT @Stats
  $ runMaybeT
  $ flip execStateT initialState
  $ flip runReaderT ProcessingEnv{workerEnv=Nothing, printMode=Print "DBG: ", catchingUp=CaughtUpWithNode, currentSlot = 1}
  $ do
    let (tx2Events, _) = firstDanglingMatchScriptInput2TxEvents
        (tx1Events, _) = firstDanglingMatchScriptInput1TxEvents
        (tx3Events, _) = firstDanglingMatchTxEvents

    traverse_ performStep [tx2Events, tx1Events, tx3Events]

performStep :: (MonadIO m, MonadState MuesliState m, MonadReader ProcessingEnv m, MonadWriter Stats m) => [TxWithEvents MuesliVersion] -> m ()
performStep events = do
  potentialMatches <- join <$> traverse applyTx events
  flip traverse_ potentialMatches $ \(order, e@NewOutputEvent{neTransaction}, matches) -> do
    liftIO $ putStrLn $ "Known order (" <> show (length matches) <> " matches) from " <> show neTransaction <> ": " <> show order
    case validateKnownOrder order e of
      Failure errs -> liftIO $ do
        putStrLn "VALIDATION FAILED"
        traverse_ putStrLn errs
      _ -> liftIO $ putStrLn "VALID"
    flip traverse_ matches $ \match -> do
      liftIO $ putStrLn $ " " <> show match

  traverse_ (uncurry insertMatch) (fmap (\(a, b, _) -> (a, b)) potentialMatches)
  printState
  liftIO $ putStrLn "Order locations:"
  use txInLocations >>= \mp -> do
    flip traverse_ (Map.toList mp) $ \(txIn, loc) -> do
      liftIO $ putStrLn $ show txIn <> " at " <> show loc
  liftIO $ putStrLn ""
  liftIO $ putStrLn "Potential orders:"
  use potentialOrders >>= \mp -> do
    flip traverse_ (Map.toList mp) $ \(bidC, mp') -> do
      liftIO $ putStrLn $ " " <> show bidC <> " (" <> show (IntMap.size mp') <> " entries):"
      flip traverse_ (IntMap.toList mp') $ \(bidQ, mp'') -> do
        liftIO $ putStrLn $ "  " <> show bidQ <> " (" <> show (Map.size mp'') <> " entries):"
        flip traverse_ (Map.toList mp'') $ \(askC, mp''') -> do
          liftIO $ putStrLn $ "   " <> show askC <> " (" <> show (IntMap.size mp''') <> " entries):"
          flip traverse_ (IntMap.toList mp''') $ \(askQ, txIns) -> do
            liftIO $ putStrLn $ "    " <> show askQ <> " (" <> show (Map.size txIns) <> " tx ins):"
            flip traverse_ (Map.toList txIns) $ \(txIn, _) -> liftIO $ putStrLn $ "     " <> show txIn

  liftIO $ putStrLn $ "-+-+-+-+-+-+-+-+-"
