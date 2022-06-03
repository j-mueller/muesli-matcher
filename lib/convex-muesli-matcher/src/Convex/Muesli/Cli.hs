{-# LANGUAGE NamedFieldPuns #-}
module Convex.Muesli.Cli(
    runMain
    ) where

import Cardano.Api qualified as CAPI
import Control.Concurrent (forkIO)
import Control.Concurrent.STM qualified as STM
import Control.Monad (void)
import Control.Monad.Trans.Except (runExceptT)
import Convex.Muesli.Config (Config (..), configParser, mkEnv)
import Convex.Muesli.Constants qualified as Constants
import Convex.Muesli.MatchWorker qualified as Worker
import Convex.Muesli.NodeClient qualified as NC
import Convex.Muesli.Stats qualified as Stats
import Convex.Muesli.Webserver (ServerArgs (..), startServer)
import Convex.NodeClient qualified as NC
import Data.Foldable (traverse_)
import Data.Text qualified as Text
import Options.Applicative (customExecParser, disambiguate, helper, idm, info, prefs, showHelpOnEmpty, showHelpOnError)

runMain :: IO ()
runMain = do
  Config{cardanoNodeConfigFile, cardanoNodeSocket, cardanoWalletConfig, webserverPort}  <- customExecParser
                (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                (info (helper <*> configParser) idm)
  let syncPoints = [Constants.muesliStart]

  workerEnv <- Worker.initialiseWorkerEnv

  walletConnections <- traverse mkEnv cardanoWalletConfig
  allStats <- STM.newTVarIO mempty
  timedStats <- STM.newTVarIO Stats.emptyStats
  void $ forkIO $ do
    putStrLn $ "Starting webserver on port " <> show webserverPort
    startServer ServerArgs{svAllStats = allStats, svTimedStats = timedStats, svPort = webserverPort}
  let client connectInfo env = do
        traverse_ (Worker.startHandlingMatches connectInfo workerEnv) walletConnections
        return (NC.resumingClient syncPoints (const $ NC.muesliClient allStats timedStats (Just workerEnv) env))
  result <- runExceptT (NC.runNodeClient cardanoNodeConfigFile cardanoNodeSocket client)
  case result of
    Left err -> do
      putStrLn "Error in runNodeClient"
      putStrLn (Text.unpack $ CAPI.renderInitialLedgerStateError err)
    Right () -> do
      putStrLn "runNodeClient successful."
