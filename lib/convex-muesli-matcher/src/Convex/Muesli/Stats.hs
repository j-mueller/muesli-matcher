{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-| Statistics collected by the muesli matchmaker
-}
module Convex.Muesli.Stats(
  -- * Actual statistics
  Stats(..),
  prettyStats,
  totalEvents,

  -- ** Lenses
  failedToDecodeMetadata,
  totalMatchesFound,
  validMatchesFound,
  invalidMatchesFound,
  ordersFulfilledTotal,
  ordersFulfilledSelf,
  ordersCancelled,
  validMatchTxnsSubmitted,
  invalidMatchTxnsSubmitted,
  slotRange,
  totalErrors,
  balanceTxFailed,
  otherErrors,
  matchesRetried,
  numberOfActiveMatches,
  txnsNotProfitable,
  txnsPostedByWallet,
  ordersFulfilledSelfAge,
  ordersFulfilledTotalAge,
  txnSubmitBlockfrostFailed,
  txnSubmitBlockfrost,

  -- ** Counting
  countEvents,
  countOrderCancelled,
  countOrderFulfilledTotal,
  countOrderFulfilledSelf,
  countMetadataFailed,
  countTotalMatches,
  countInvalidMatch,
  countValidMatch,
  countValidMatchTxnSubmitted,
  countInvalidMatchTxnSubmitted,
  countError,
  countBalanceTxFailed,
  countOtherError,
  countRetryMatches,
  countTxnNotProfitable,
  countTxnPosted,
  countTxnBlockfrost,
  countTxnBlockfrostFailed,
  measureActiveMatches,

  -- * Aggregated statistics
  MuesliStats,
  emptyStats,
  prepend,
  fromBlockHeader,

  -- ** Queries
  StatsAt(..),
  lastHour,
  lastDay,
  lastWeek,
  stats,
  toList,

  -- ** Debugging
  debugStats,
  debugStatsFromList
  ) where

import Cardano.Api (AsType (AsHash), BlockHeader, BlockNo, Hash, SlotNo)
import Cardano.Api qualified as C
import Cardano.Wallet.Primitive.Types (WalletId)
import Control.Lens (at, makeLenses, (&), (.~), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.FingerTree (FingerTree, Measured (..), split, (<|))
import Data.Foldable (foldl')
import Data.Foldable qualified as F
import Data.FormatN qualified as F
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Class (ToText (..))
import GHC.Generics (Generic)

data Stats =
  Stats
    { _totalEvents               :: !Int
    , _failedToDecodeMetadata    :: !Int
    , _totalMatchesFound         :: !Int
    , _validMatchesFound         :: !Int
    , _invalidMatchesFound       :: !Int
    , _ordersFulfilledTotal      :: !Int
    , _ordersFulfilledTotalAge   :: !Int -- ^ seconds
    , _ordersFulfilledSelf       :: !Int
    , _ordersFulfilledSelfAge    :: !Int -- ^ seconds
    , _ordersCancelled           :: !Int
    , _validMatchTxnsSubmitted   :: !Int
    , _invalidMatchTxnsSubmitted :: !Int
    , _slotRange                 :: !(StrictMaybe (SlotNo, SlotNo))
    , _totalErrors               :: !Int
    , _balanceTxFailed           :: !Int
    , _otherErrors               :: !Int
    , _matchesRetried            :: !Int
    , _numberOfActiveMatches     :: !Int
    , _txnsNotProfitable         :: !Int
    , _txnsPostedByWallet        :: !(Map.Map Text Int)
    , _txnSubmitBlockfrost       :: !Int
    , _txnSubmitBlockfrostFailed :: !Int
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (ToJSON, FromJSON)

makeLenses ''Stats

tshow :: forall a. Show a => a -> Text
tshow = Text.pack . show

parens :: Text -> Text
parens t = "(" <> t <> ")"

countEvents :: Int -> Stats
countEvents i = mempty & totalEvents .~ i

countOrderCancelled :: Stats
countOrderCancelled = mempty & ordersCancelled .~ 1

countOrderFulfilledTotal :: SlotNo -> Stats
countOrderFulfilledTotal (C.SlotNo age) = mempty & ordersFulfilledTotal .~ 1 & ordersFulfilledTotalAge .~ fromIntegral age

countOrderFulfilledSelf :: SlotNo -> Stats
countOrderFulfilledSelf (C.SlotNo age) = mempty & ordersFulfilledSelf .~ 1 & ordersFulfilledSelfAge .~ fromIntegral age

countMetadataFailed :: Stats
countMetadataFailed = mempty & failedToDecodeMetadata .~ 1

countTotalMatches :: Int -> Stats
countTotalMatches i = mempty & totalMatchesFound .~ i

countInvalidMatch :: Stats
countInvalidMatch = mempty & invalidMatchesFound .~ 1

countError :: Stats
countError = mempty & totalErrors .~ 1

countOtherError :: Stats
countOtherError = mempty & otherErrors .~ 1

countValidMatchTxnSubmitted :: Stats
countValidMatchTxnSubmitted = mempty & validMatchTxnsSubmitted .~ 1

countInvalidMatchTxnSubmitted :: Stats
countInvalidMatchTxnSubmitted = mempty & invalidMatchTxnsSubmitted .~ 1

countValidMatch :: Stats
countValidMatch = mempty & validMatchesFound .~ 1

countBalanceTxFailed :: Stats
countBalanceTxFailed = mempty & balanceTxFailed .~ 1

countRetryMatches :: Int -> Stats
countRetryMatches i = mempty & matchesRetried .~ i

countTxnNotProfitable :: Stats
countTxnNotProfitable = mempty & txnsNotProfitable .~ 1

measureActiveMatches :: Int -> Stats
measureActiveMatches i = mempty & numberOfActiveMatches .~ i

countTxnPosted :: WalletId -> Stats
countTxnPosted i = mempty & txnsPostedByWallet . at (toText i) ?~ 1

countTxnBlockfrost :: Stats
countTxnBlockfrost = mempty & txnSubmitBlockfrost .~ 1

countTxnBlockfrostFailed :: Stats
countTxnBlockfrostFailed = mempty & txnSubmitBlockfrostFailed .~ 1

-- | Render the statistics
prettyStats :: Stats -> Text
prettyStats Stats{_totalEvents, _totalMatchesFound, _validMatchesFound, _ordersFulfilledTotal, _ordersFulfilledSelf, _ordersCancelled, _validMatchTxnsSubmitted, _invalidMatchTxnsSubmitted, _failedToDecodeMetadata, _totalErrors, _balanceTxFailed, _otherErrors, _matchesRetried, _numberOfActiveMatches, _txnsNotProfitable, _txnsPostedByWallet, _ordersFulfilledSelfAge, _ordersFulfilledTotalAge, _txnSubmitBlockfrost, _txnSubmitBlockfrostFailed} =
  let invalidMatches = _totalMatchesFound - _validMatchesFound
      selfPercent :: Double = if _ordersFulfilledTotal == 0 then 0 else fromIntegral _ordersFulfilledSelf / fromIntegral _ordersFulfilledTotal
      walletCounts = Text.unlines (fmap (\(wallet, txns) -> "  " <> wallet <> ": " <> tshow txns) $ Map.toList _txnsPostedByWallet)
      selfAge :: Double
      selfAge = if _ordersFulfilledSelf == 0 then 0 else fromIntegral _ordersFulfilledSelfAge / fromIntegral _ordersFulfilledSelf
      otherAge :: Double
      otherAge = if _ordersFulfilledTotal == 0 then 0 else fromIntegral (_ordersFulfilledTotalAge - _ordersFulfilledSelfAge) / fromIntegral (_ordersFulfilledTotal - _ordersFulfilledSelf)
  in Text.unlines
      [ "Total events recorded: " <> tshow _totalEvents
      , "Matches found:         " <> tshow _totalMatchesFound <> " " <> parens (tshow _validMatchesFound <> " valid / " <> tshow invalidMatches <> " invalid")
      , "Orders fulfilled:      " <> tshow _ordersFulfilledTotal <> " " <> parens (tshow _ordersFulfilledSelf <> " / " <> F.percent (Just 1) selfPercent <> " self")
      , "Fulfilment time (s):   " <> F.fixed (Just 1) selfAge <> " (self) / " <> F.fixed (Just 1) otherAge <> " (other)"
      , "Total errors:          " <> tshow _totalErrors <> " " <> parens (tshow _balanceTxFailed <> " balanceTx, " <> tshow _failedToDecodeMetadata <> " metadata, " <> tshow _txnsNotProfitable <> " not profitable, " <> tshow _otherErrors <> " other")
      , "Blockfrost submits:    " <> tshow _txnSubmitBlockfrost <> " " <> parens (tshow _txnSubmitBlockfrostFailed <> " failed")
      , "Matches submitted by wallet: " <> parens (tshow _validMatchTxnsSubmitted <> " total valid / " <> tshow _invalidMatchTxnsSubmitted <> " invalid")
      , walletCounts
      ]

instance Semigroup Stats where
  l <> r =
    Stats
      { _totalEvents               = _totalEvents l + _totalEvents r
      , _failedToDecodeMetadata    = _failedToDecodeMetadata l + _failedToDecodeMetadata r
      , _totalMatchesFound         = _totalMatchesFound l + _totalMatchesFound r
      , _validMatchesFound         = _validMatchesFound l + _validMatchesFound r
      , _invalidMatchesFound       = _invalidMatchesFound l + _invalidMatchesFound r
      , _ordersFulfilledSelf       = _ordersFulfilledSelf l + _ordersFulfilledSelf r
      , _ordersFulfilledSelfAge    = _ordersFulfilledSelfAge l + _ordersFulfilledSelfAge r
      , _ordersFulfilledTotal      = _ordersFulfilledTotal l + _ordersFulfilledTotal r
      , _ordersFulfilledTotalAge   = _ordersFulfilledTotalAge l + _ordersFulfilledTotalAge r
      , _ordersCancelled           = _ordersCancelled l + _ordersCancelled r
      , _validMatchTxnsSubmitted   = _validMatchTxnsSubmitted l + _validMatchTxnsSubmitted r
      , _invalidMatchTxnsSubmitted = _invalidMatchTxnsSubmitted l + _invalidMatchTxnsSubmitted r
      , _slotRange =
          case (_slotRange l, _slotRange r) of
            (SNothing, x)                            -> x
            (x, SNothing)                            -> x
            (SJust (fromL, toL), SJust (fromR, toR)) -> SJust (min fromL fromR, max toL toR)
      , _totalErrors = _totalErrors l + _totalErrors r
      , _balanceTxFailed = _balanceTxFailed l + _balanceTxFailed r
      , _otherErrors = _otherErrors l + _otherErrors r
      , _matchesRetried = _matchesRetried l + _matchesRetried r
      , _numberOfActiveMatches = _numberOfActiveMatches l `max` _numberOfActiveMatches r
      , _txnsNotProfitable = _txnsNotProfitable l + _txnsNotProfitable r
      , _txnsPostedByWallet = Map.unionWith (+) (_txnsPostedByWallet l) (_txnsPostedByWallet r)
      , _txnSubmitBlockfrost = _txnSubmitBlockfrost l + _txnSubmitBlockfrost r
      , _txnSubmitBlockfrostFailed = _txnSubmitBlockfrostFailed l + _txnSubmitBlockfrostFailed r
      }

instance Monoid Stats where
  mempty =
    Stats
      { _totalEvents = 0
      , _failedToDecodeMetadata = 0
      , _totalMatchesFound = 0
      , _validMatchesFound = 0
      , _invalidMatchesFound = 0
      , _ordersFulfilledTotal = 0
      , _ordersFulfilledTotalAge = 0
      , _ordersFulfilledSelf = 0
      , _ordersFulfilledSelfAge = 0
      , _ordersCancelled = 0
      , _validMatchTxnsSubmitted = 0
      , _invalidMatchTxnsSubmitted = 0
      , _slotRange = SNothing
      , _totalErrors = 0
      , _otherErrors = 0
      , _balanceTxFailed = 0
      , _matchesRetried = 0
      , _numberOfActiveMatches = 0
      , _txnsNotProfitable = 0
      , _txnsPostedByWallet = mempty
      , _txnSubmitBlockfrost = 0
      , _txnSubmitBlockfrostFailed = 0
      }

data StatsAt =
  StatsAt
    { _blockNo   :: BlockNo
    , _slotNo    :: SlotNo
    , _blockHash :: Hash BlockHeader
    , _stats     :: Stats
    } deriving stock (Show)

fromBlockHeader :: BlockHeader -> Stats -> StatsAt
fromBlockHeader (C.BlockHeader _slotNo _blockHash _blockNo) s =
  let _stats = s & slotRange .~ SJust (_slotNo, _slotNo)
  in StatsAt{_blockNo, _slotNo, _blockHash, _stats}

instance Measured Stats StatsAt where
  measure = _stats

newtype MuesliStats = MuesliStats{ unMuesliStats :: FingerTree Stats StatsAt }
  deriving stock (Show)

prepend ::  StatsAt -> MuesliStats -> MuesliStats
prepend s MuesliStats{unMuesliStats} =
  MuesliStats $ s <| unMuesliStats

emptyStats :: MuesliStats
emptyStats = MuesliStats mempty

-- | 'StatsAt' constructor for debugging purposes
debugStats :: SlotNo -> StatsAt
debugStats _slotNo =
  let _stats = mempty & totalEvents .~ 1 & slotRange .~ SJust (_slotNo, _slotNo)
      _blockHash = fromMaybe (error "debugStats") (C.deserialiseFromRawBytesHex (AsHash (C.proxyToAsType (Proxy :: Proxy BlockHeader))) "f90517fd9fb9194b1ef6f9c1d147c0f175c3b29604542692e3d06bf13403aacf")
  in StatsAt{_slotNo, _blockHash, _blockNo = 0, _stats}

debugStatsFromList :: [SlotNo] -> MuesliStats
debugStatsFromList = foldl' (\s slot -> flip prepend s (debugStats slot)) emptyStats . sort

splitLastNSlots :: Int -> MuesliStats -> (MuesliStats, MuesliStats)
splitLastNSlots n (MuesliStats s) =
  case (_slotRange $ measure s) of
    SNothing -> (MuesliStats mempty, MuesliStats s)
    SJust (_, maxSlot) ->
      let cutoffPoint :: SlotNo = maxSlot - (fromIntegral n)
          f Stats{_slotRange = SNothing}            = True
          f Stats{_slotRange = SJust (minSlot', _)} = minSlot' < cutoffPoint
          (this, that) = split f s
      in (MuesliStats this, MuesliStats that)

-- | Split a 'MuesliStats' value into one with the values for the last hour
--   and one with the rest
lastHour :: MuesliStats -> (MuesliStats, MuesliStats)
lastHour = splitLastNSlots (60 * 60)

-- | Split a 'MuesliStats' value into one with the values for the last day
--   and one with the rest
lastDay :: MuesliStats -> (MuesliStats, MuesliStats)
lastDay = splitLastNSlots (60 * 60 * 24)

-- | Split a 'MuesliStats' value into one with the values for the last week
--   and one with the rest
lastWeek :: MuesliStats -> (MuesliStats, MuesliStats)
lastWeek = splitLastNSlots (60 * 60 * 24 * 7)

-- | The raw stats
stats :: MuesliStats -> Stats
stats = measure . unMuesliStats

-- | A list with all stats that were recorded
toList :: MuesliStats -> [StatsAt]
toList = F.toList . unMuesliStats
