{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Convex.Muesli.Blockfrost(
  txInputAddresses,
  submitTxn
  ) where

import Cardano.Api (Address, ShelleyAddr, TxId)
import Cardano.Api qualified as C
import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (join)
import Data.Aeson (FromJSON, Value, decode)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wreq (header, responseBody)
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (Postable (..))

data TxnInput =
  TxnInput
    { address      :: String
    , amount       :: [Value]
    , tx_hash      :: String
    , output_index :: Int
    , data_hash    :: Maybe String
    , collateral   :: Bool
    } deriving stock (Generic)
      deriving anyclass FromJSON

data TxnsUtxosResp =
  TxnsUtxosResp
    { hash    :: String
    , inputs  :: [TxnInput]
    , outputs :: [Value]
    } deriving stock (Generic)
      deriving anyclass FromJSON

projectID :: ByteString
projectID = "XXXX"

-- | Find all addresses that the tx spent inputs from
txInputAddresses :: TxId -> IO [Address ShelleyAddr]
txInputAddresses txId = fmap (either (const []) id) $ try @SomeException $ do
  let opts = Wreq.defaults & header "project_id" .~ [projectID]
  r <- Wreq.getWith opts $ "https://cardano-mainnet.blockfrost.io/api/v0/txs/" <> filter ((/=) '\"') (show txId) <> "/utxos"
  case decode (r ^. responseBody) of
    Nothing -> do
      putStrLn "Failed to decode response"
      return []
    Just TxnsUtxosResp{inputs} -> do
      let addresses = mapMaybe (C.deserialiseAddress (C.proxyToAsType $ Proxy @(Address ShelleyAddr)) . Text.pack) (address <$> inputs)
      return addresses

newtype CBORByteString = CBORByteString { getCBORByteString :: ByteString }

instance Postable CBORByteString where
  postPayload = payload "application/cbor" . HTTP.RequestBodyBS . getCBORByteString

submitTxn :: ByteString -> IO (Either String ())
submitTxn bs = fmap (join . first show) $ try @SomeException $ do
  let opts = Wreq.defaults
              & header "project_id" .~ [projectID]
  r <- Wreq.postWith opts "https://cardano-mainnet.blockfrost.io/api/v0/tx/submit" (CBORByteString bs)
  let st = HTTP.responseStatus r
  if HTTP.statusCode st == 200
    then do
      putStrLn $ "Blockfrost.submitTxn: SUCCESS"
      pure (Right ())
    else do
      putStrLn $ "Blockfrost.submitTxn: " <> show st
      pure (Left $ show st)

payload :: ByteString -> HTTP.RequestBody -> HTTP.Request -> IO HTTP.Request
payload ct body req =
  let hds = HTTP.requestHeaders req
      newHeaders = hds ++ [("Content-Type", ct)]
  in return $ req { HTTP.requestBody = body, HTTP.requestHeaders = newHeaders }
