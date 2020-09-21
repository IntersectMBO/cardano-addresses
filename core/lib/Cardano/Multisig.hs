{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Multisig
    (
      MultisigScript (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..), XPub )
import Cardano.Address.Style.Shelley
    ( Shelley )
import Control.DeepSeq
    ( NFData )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )

-- | A 'MultisigScript' type represents multi signature script. The script embodies conditions
-- that need to be satisfied to make it valid.
--
-- @since 3.0.0
data MultisigScript =
      RequireSignatureOf (Shelley 'MultisigK XPub)
    | RequireAllOf [MultisigScript]
    | RequireAnyOf [MultisigScript]
    | RequireMOf Word8 [MultisigScript]
    deriving stock (Generic, Show, Eq)

instance NFData MultisigScript
