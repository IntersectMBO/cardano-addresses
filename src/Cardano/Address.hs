{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

module Cardano.Address
    ( -- * Address
      Address
    , PaymentAddress (..)
    , DelegationAddress (..)
    , unsafeMkAddress

      -- * Conversion From / To Text
    , base58
    , fromBase58
    , bech32
    , fromBech32

      -- * Network Discrimination
    , HasNetworkDiscriminant (..)
    , NetworkDiscriminantByron (..)
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
    ( Depth (..), XPub )
import Cardano.Codec.Cbor
    ( decodeAddress, deserialiseCbor )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( (<=<) )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
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
--
-- @since 1.0.0
newtype Address = Address
    { unAddress :: ByteString
    } deriving stock (Generic, Show, Eq, Ord)
instance NFData Address

-- Unsafe constructor for easily lifting bytes inside an 'Address'.
--
-- /!\ Use at your own risks.
unsafeMkAddress :: ByteString -> Address
unsafeMkAddress = Address

-- | Encode an 'Address' to a base58 'Text'.
--
-- @since 1.0.0
base58 :: Address -> Text
base58 = T.decodeUtf8 . encodeBase58 bitcoinAlphabet . unAddress

-- | Decode a base58-encoded 'Text' into an 'Address'
--
-- @since 1.0.0
fromBase58 :: Text -> Maybe Address
fromBase58 =
    deserialiseCbor (unsafeMkAddress <$> decodeAddress)
   <=<
    decodeBase58 bitcoinAlphabet . T.encodeUtf8

-- | Encode an 'Address' to bech32 'Text', using @addr@ as a human readable prefix.
--
-- @since 1.0.0
bech32 :: Address -> Text
bech32 = Bech32.encodeLenient hrp . Bech32.dataPartFromBytes . unAddress
  where
    hrp = [Bech32.humanReadablePart|addr|]

-- | Decode a bech32-encoded  'Text' into an 'Address'
--
-- @since 1.0.0
fromBech32 :: Text -> Maybe Address
fromBech32 =
    fmap unsafeMkAddress . Bech32.dataPartToBytes
   <=<
    either (const Nothing) (Just . snd) . Bech32.decodeLenient

-- | Encoding of addresses for certain key types and backend targets.
--
-- @since 1.0.0
class HasNetworkDiscriminant key => PaymentAddress key where
    -- | Convert a public key to a payment 'Address' valid for the given
    -- network discrimination.
    --
    -- @since 1.0.0
    paymentAddress :: NetworkDiscriminant key -> key 'AddressK XPub -> Address

-- | Encoding of delegation addresses for certain key types and backend targets.
--
-- @since 1.0.1
class PaymentAddress key
    => DelegationAddress key where
    -- | Convert a public key and a staking key to a delegation 'Address' valid
    -- for the given network discrimination. Funds sent to this address will be
    -- delegated according to the delegation settings attached to the delegation
    -- key.
    --
    -- @since 1.0.1
    delegationAddress
        :: NetworkDiscriminant key
        ->  key 'AddressK XPub
            -- ^ Payment key
        ->  key 'AddressK XPub
            -- ^ Staking key / Reward account
        -> Address

class HasNetworkDiscriminant (key :: Depth -> * -> *) where
    type NetworkDiscriminant key :: *
    type NetworkNumber key :: *

    requiredInAddress :: NetworkDiscriminant key -> Bool
    networkNumber :: NetworkDiscriminant key -> NetworkNumber key

-- | Describe required network discrimination for Byron era. See also the available
-- smart-constructors if you are not sure about what to do with this:
--
-- - 'mainnetDiscriminant'
-- - 'stagingDiscriminant'
-- - 'testnetDiscriminant'
--
-- @since 1.0.0
data NetworkDiscriminantByron
    = RequiresNoMagic
    | RequiresMagic ProtocolMagic
    deriving (Generic, Show, Eq)
instance NFData NetworkDiscriminantByron

-- | Required network discrimination settings for mainnet
--
-- @since 1.0.0
mainnetDiscriminant :: NetworkDiscriminantByron
mainnetDiscriminant = RequiresNoMagic

-- | Required network discrimination settings for staging
--
-- @since 1.0.0
stagingDiscriminant :: NetworkDiscriminantByron
stagingDiscriminant = RequiresNoMagic

-- | Required network discrimination settings for testnet
--
-- @since 1.0.0
testnetDiscriminant :: NetworkDiscriminantByron
testnetDiscriminant = RequiresMagic testnetMagic

-- | Magic constant associated with a given network. This is mainly used in two
-- places:
--
-- (1) In 'Address' payloads, to discriminate addresses between networks.
-- (2) At the network-level, when doing handshake with nodes.
--
-- @since 1.0.0
newtype ProtocolMagic
    = ProtocolMagic Word32
    deriving (Generic, Show, Eq)
instance NFData ProtocolMagic

-- | Hard-coded 'ProtocolMagic' for Cardano MainNet
--
-- @since 1.0.0
mainnetMagic :: ProtocolMagic
mainnetMagic =  ProtocolMagic 764824073

-- | Hard-coded 'ProtocolMagic' for Cardano Staging
--
-- @since 1.0.0
stagingMagic :: ProtocolMagic
stagingMagic = ProtocolMagic 633343913

-- | Hard-coded 'ProtocolMagic' for Cardano standard TestNet
--
-- @since 1.0.0
testnetMagic :: ProtocolMagic
testnetMagic = ProtocolMagic 1097911063
