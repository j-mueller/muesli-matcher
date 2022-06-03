{-# LANGUAGE OverloadedStrings #-}
module Convex.Muesli.Keys(ownStakingKeys) where

import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Crypto qualified as C
import Convex.Muesli.Constants (getStakingKey)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

ownStakingKeys :: Set.Set (L.StakeReference C.StandardCrypto)
ownStakingKeys =
  Set.fromList
    $ fmap (fromMaybe (error "ownStakingKey") . getStakingKey)
      [ "XXXX"
      ]
