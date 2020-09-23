{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Multisig
    (
      MultisigScript (..)
    , VerificationKeyHash (..)
    , fromVerificationKey
    , unsafeVerificationKeyHash
    ) where

import Prelude

import Cardano.Address
    ( invariantSize )
import Cardano.Address.Derivation
    ( Depth (..), XPub )
import Cardano.Address.Style.Shelley
    ( Shelley, blake2b224, pubkeyHashSize )
import Control.DeepSeq
    ( NFData )
import Data.ByteString
    ( ByteString )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )

-- | A 'MultisigScript' type represents multi signature script. The script embodies conditions
-- that need to be satisfied to make it valid.
--
-- @since 3.0.0
data MultisigScript =
      RequireSignatureOf VerificationKeyHash
    | RequireAllOf [MultisigScript]
    | RequireAnyOf [MultisigScript]
    | RequireMOf Word8 [MultisigScript]
    deriving stock (Generic, Show, Eq)

instance NFData MultisigScript

newtype VerificationKeyHash = VerificationKeyHash ByteString
    deriving (Generic, Show, Eq)

instance NFData VerificationKeyHash

fromVerificationKey :: Shelley 'MultisigK XPub -> VerificationKeyHash
fromVerificationKey = VerificationKeyHash . blake2b224

unsafeVerificationKeyHash :: ByteString -> VerificationKeyHash
unsafeVerificationKeyHash = VerificationKeyHash . invariantSize pubkeyHashSize
