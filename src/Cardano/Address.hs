{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK prune #-}

module Cardano.Address
    ( -- * Address
      Address
    , PaymentAddress (..)
    , unsafeMkAddress

      -- * Conversion To Text
    , base58
    , bech32

      -- * Network Discrimination
    , NetworkDiscriminant (..)
    , mainnetDiscriminant
    , stagingDiscriminant
    , testnetDiscriminant

      -- * Protocol Magic
    , ProtocolMagic (..)
    , mainnetMagic
    , stagingMagic
    , testnetMagic
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..) )
import Cardano.Crypto.Wallet
    ( XPub )
import Control.DeepSeq
    ( NFData )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, encodeBase58 )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.Text.Encoding as T

-- | An 'Address' type representing 'Cardano' addresses. Internals are
-- irrevelant to the user.
newtype Address = Address
    { unAddress :: ByteString
    } deriving stock (Generic, Show, Eq, Ord)
      deriving newtype (ByteArrayAccess)
instance NFData Address

-- Unsafe constructor for easily lifting bytes inside an 'Address'.
--
-- /!\ Use at your own risks.
unsafeMkAddress :: ByteString -> Address
unsafeMkAddress = Address

-- | Encode an address to base58.
base58 :: Address -> Text
base58 = T.decodeUtf8 . encodeBase58 bitcoinAlphabet . unAddress

-- | Encode an address to bech32, using @addr@ as a human readable prefix.
bech32 :: Address -> Text
bech32 = Bech32.encodeLenient hrp . Bech32.dataPartFromBytes . unAddress
  where
    hrp = [Bech32.humanReadablePart|addr|]

-- | Encoding of addresses for certain key types and backend targets.
class PaymentAddress key where
    -- | Convert a public key to a payment 'Address' valid for the given
    -- network discrimination.
    --
    -- Note that 'paymentAddress' is ambiguous and requires therefore a type
    -- application.
    paymentAddress :: NetworkDiscriminant -> key 'AddressK XPub -> Address

-- | Available network options.
data NetworkDiscriminant
    = RequiresNoMagic
    | RequiresMagic ProtocolMagic
    deriving (Generic, Show, Eq)
instance NFData NetworkDiscriminant

-- | Required network discrimination settings for mainnet
mainnetDiscriminant :: NetworkDiscriminant
mainnetDiscriminant = RequiresNoMagic

-- | Required network discrimination settings for staging
stagingDiscriminant :: NetworkDiscriminant
stagingDiscriminant = RequiresNoMagic

-- | Required network discrimination settings for testnet
testnetDiscriminant :: NetworkDiscriminant
testnetDiscriminant = RequiresMagic testnetMagic

-- | Magic constant associated to a given network. This is mainly use in two
-- places:
--
-- a) In 'Address' payloads, to discriminate addresses between networks.
-- b) At the network-level, when doing handshake with nodes.
newtype ProtocolMagic
    = ProtocolMagic { getProtocolMagic :: Word32 }
    deriving (Generic, Show, Eq)
instance NFData ProtocolMagic

-- | Hard-coded 'ProtocolMagic' for Cardano MainNet
mainnetMagic :: ProtocolMagic
mainnetMagic =  ProtocolMagic 764824073

-- | Hard-coded 'ProtocolMagic' for Cardano Staging
stagingMagic :: ProtocolMagic
stagingMagic = ProtocolMagic 633343913

-- | Hard-coded 'ProtocolMagic' for Cardano standard TestNet
testnetMagic :: ProtocolMagic
testnetMagic = ProtocolMagic 1097911063
