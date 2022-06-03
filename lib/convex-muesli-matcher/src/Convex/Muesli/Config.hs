{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Convex.Muesli.Config(
  Config(..),
  CardanoWalletConfig(..),
  configParser,
  walletConfigParser,
  -- * Wallet connection
  CardanoWalletEnv(..),
  mkEnv
  ) where

import Cardano.Wallet.Primitive.AddressDerivation (Passphrase)
import Cardano.Wallet.Primitive.Types (WalletId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as Text
import Data.Text.Class (FromText (..), fromText)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP.Client
import Options.Applicative (Parser, auto, help, long, option, strOption, value)
import Options.Applicative.Types (fromM, manyM)
import Servant.Client (ClientEnv)
import Servant.Client qualified
import Servant.Client.Core.BaseUrl qualified as BaseUrl
import System.Exit (exitFailure)

data CardanoWalletConfig =
  CardanoWalletConfig
    { cardanoWalletUri        :: String
    , cardanoWalletId         :: String
    , cardanoWalletPassPhrase :: String
    }
      deriving stock (Eq, Ord, Show, Generic)
      deriving anyclass (ToJSON, FromJSON)

data Config =
  Config
      { cardanoNodeConfigFile :: FilePath
      , cardanoNodeSocket     :: FilePath
      , webserverPort         :: Int
      , cardanoWalletConfig   :: [CardanoWalletConfig]
      }
      deriving stock (Eq, Ord, Show, Generic)
      deriving anyclass (ToJSON, FromJSON)

walletConfigParser :: Parser CardanoWalletConfig
walletConfigParser =
  CardanoWalletConfig
    <$> strOption (long "wallet-uri" <> help "Cardano wallet URI")
    <*> strOption (long "wallet-id" <> help "Cardano wallet ID")
    <*> strOption (long "wallet-passphrase" <> help "Cardano wallet passphrase")


configParser :: Parser Config
configParser =
  Config
      <$> strOption (long "node-config" <> help "Cardano node config JSON file")
      <*> strOption (long "node-socket" <> help "Cardano node socket")
      <*> option auto (long "port" <> help "Port of the webserver" <> value 8097)
      <*> fromM (manyM walletConfigParser)

data CardanoWalletEnv =
  CardanoWalletEnv
    { cweClient     :: ClientEnv
    , cweWalletId   :: WalletId
    , cwePassphrase :: Passphrase "lenient"
    }

instance Show CardanoWalletEnv where
  show CardanoWalletEnv{cweWalletId} = "CardanoWalletEnv{cweWalletId=" <> show cweWalletId <> "}"

{-| Initialise a 'CardanoWalletEnv' using the config values
-}
mkEnv :: CardanoWalletConfig -> IO CardanoWalletEnv
mkEnv CardanoWalletConfig{cardanoWalletUri, cardanoWalletId, cardanoWalletPassPhrase} = do
    putStrLn $ "Using cardano-wallet url " <> cardanoWalletUri
    baseUrl <- BaseUrl.parseBaseUrl cardanoWalletUri
    cweClient <- flip Servant.Client.mkClientEnv baseUrl <$> HTTP.Client.newManager HTTP.Client.defaultManagerSettings
    cweWalletId <- case fromText (Text.pack cardanoWalletId) of
      Left err -> do
        putStrLn "Failed to parse wallet-id"
        print err
        exitFailure
      Right i -> return i
    cwePassphrase <- case fromText (Text.pack cardanoWalletPassPhrase) of
      Left err -> do
        putStrLn "Failed to decode passphrase"
        print err
        exitFailure
      Right p -> return p
    return CardanoWalletEnv{cweClient, cweWalletId, cwePassphrase}
