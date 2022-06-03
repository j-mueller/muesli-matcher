{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-| Servant server for stats
-}
module Convex.Muesli.Webserver(
  ServerArgs(..),
  startServer,
  API,
  StatsResponse(..)
  )  where

import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Monad.IO.Class (MonadIO (..))
import Convex.Muesli.Stats (MuesliStats, Stats)
import Convex.Muesli.Stats qualified as Stats
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp qualified
import Servant (Application, Get, JSON, Server, serve, (:>))

data ServerArgs =
  ServerArgs
    { svAllStats   :: TVar Stats
    , svTimedStats :: TVar MuesliStats
    , svPort       :: Int
    }

data StatsResponse =
  StatsResponse
    { lastHour :: Stats
    , lastDay  :: Stats
    , lastWeek :: Stats
    , allTime  :: Stats
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type API = "stats" :> Get '[JSON] StatsResponse

server :: ServerArgs -> Server API
server ServerArgs{svAllStats, svTimedStats} =
  liftIO $ atomically $ do
    (allTime, timed) <- (,) <$> readTVar svAllStats <*> readTVar svTimedStats
    pure
      StatsResponse
        { lastHour = Stats.stats $ fst $ Stats.lastHour timed
        , lastDay  = Stats.stats $ fst $ Stats.lastDay timed
        , lastWeek = Stats.stats $ fst $ Stats.lastWeek timed
        , allTime
        }

app :: ServerArgs -> Application
app args = serve (Proxy @API) (server args)

startServer :: ServerArgs -> IO ()
startServer args@ServerArgs{svPort} =
  Network.Wai.Handler.Warp.run svPort (app args)
