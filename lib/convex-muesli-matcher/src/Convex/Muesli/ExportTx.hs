{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-| Plutus.Contract.Wallet.ExportTx
-}
module Convex.Muesli.ExportTx(
  ExportTx(..),
  ExportTxInput(..),
  ExportTxRedeemer(..),
  redeemWith,
  input,
  -- * Creating transactions for Muesli
  mkExportTx
  ) where

import Cardano.Api (AlonzoEra, Tx (..), TxBodyContent (..))
import Cardano.Api qualified as C
import Cardano.Api.Shelley (ProtocolParameters)
import Control.Monad.Except (MonadError, liftEither)
import Convex.Muesli.Constants qualified as Constants
import Convex.Muesli.KnownOrder (orderTxIn, orderTxOut)
import Convex.Muesli.Match (Match (..))
import Convex.Wallet.ExportTx (ExportTx (..), ExportTxInput (..), ExportTxRedeemer (..), input, redeemWith)

matchTx :: (MonadError C.TxBodyError m) => ProtocolParameters -> Match -> m (Tx AlonzoEra)
matchTx params Match{orderOne, orderOneEvent, orderTwo, orderTwoEvent} = do
  -- ProcessingEnv{currentSlot} <- ask
  txBody <- liftEither $ C.makeTransactionBody $
              TxBodyContent
              { txIns = [orderTxIn orderOne orderOneEvent, orderTxIn orderTwo orderTwoEvent]
              , txInsCollateral = C.TxInsCollateralNone
              , txOuts = [orderTxOut orderOne, orderTxOut orderTwo]
              , txFee = C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra 0
              -- , txValidityRange = (C.TxValidityNoLowerBound, C.TxValidityUpperBound C.ValidityUpperBoundInAlonzoEra (currentSlot + _MATCH_TX_TTL))
              , txValidityRange = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAlonzoEra)
              , txMetadata = C.TxMetadataNone
              , txAuxScripts = C.TxAuxScriptsNone
              , txExtraKeyWits = C.TxExtraKeyWitnessesNone
              , txProtocolParams = C.BuildTxWith (Just params)
              , txWithdrawals = C.TxWithdrawalsNone
              , txCertificates = C.TxCertificatesNone
              , txUpdateProposal = C.TxUpdateProposalNone
              , txMintValue = C.TxMintNone
              , txScriptValidity = C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptValid
              }
  return (C.Tx txBody [])

mkRedeemers :: Match -> [ExportTxRedeemer]
mkRedeemers Match{orderOneEvent, orderTwoEvent} =
  [ redeemWith orderOneEvent Constants.fullMatchRedeemer
  , redeemWith orderTwoEvent Constants.fullMatchRedeemer
  ]

mkLookups :: Match -> [ExportTxInput]
mkLookups Match{orderOneEvent, orderTwoEvent} =
  [ input orderOneEvent
  , input orderTwoEvent
  ]

mkExportTx :: MonadError C.TxBodyError m => ProtocolParameters -> Match -> m ExportTx
mkExportTx params match = do
  partialTx <- matchTx params match
  let lookups = mkLookups match
      redeemers = mkRedeemers match
  return ExportTx{partialTx,lookups,redeemers}
