{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

module Cardano.Address
    ( -- * Address
      Address
    , PaymentAddress (..)
    , StakeAddress (..)
    , DelegationAddress (..)
    , PointerAddress (..)
    , ChainPointer (..)
    , unsafeMkAddress
    , unAddress

      -- * Conversion From / To Text
    , base58
    , fromBase58
    , bech32
    , bech32With
    , fromBech32

      -- Internal / Network Discrimination
    , HasNetworkDiscriminant (..)
    , AddressDiscrimination (..)
    , NetworkTag (..)
    , invariantSize
    , invariantNetworkTag
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..), XPub )
import Cardano.Codec.Cbor
    ( decodeAddress, deserialiseCbor )
import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( (<=<) )
import Data.Aeson
    ( ToJSON (..), Value (..), object, (.=) )
import Data.Bits
    ( Bits (testBit) )
import Data.ByteString
    ( ByteString )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Kind
    ( Type )
import Data.Text
    ( Text )
import Data.Word
    ( Word32, Word8 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Codec.Binary.Encoding as E
import qualified Data.ByteString as BS
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
base58 = T.decodeUtf8 . encode EBase58 . unAddress

-- | Decode a base58-encoded 'Text' into an 'Address'
--
-- @since 1.0.0
fromBase58 :: Text -> Maybe Address
fromBase58 =
    deserialiseCbor (unsafeMkAddress <$> decodeAddress)
   <=<
    eitherToMaybe . E.fromBase58 . T.encodeUtf8

-- | Encode a Shelley 'Address' to bech32 'Text', using @addr@ or @addr_test@ as
-- a human readable prefix (depending on the network tag in the address).
--
-- @since 1.0.0
bech32 :: Address -> Text
bech32 addr = bech32With (addressHrp addr) addr

-- | Encode an 'Address' to bech32 'Text', using the specified human readable
-- prefix.
--
-- @since 2.0.0
bech32With :: HumanReadablePart -> Address -> Text
bech32With hrp = T.decodeLatin1 . encode (EBech32 hrp) . unAddress

-- | Decode a bech32-encoded 'Text' into an 'Address'
--
-- @since 1.0.0
fromBech32 :: Text -> Maybe Address
fromBech32 = eitherToMaybe
    . fmap (unsafeMkAddress . snd)
    . E.fromBech32 (const id)
    . T.encodeUtf8

-- | Returns the HRP for a shelley address, using the network tag.
addressHrp :: Address -> HumanReadablePart
addressHrp (Address bs) = case BS.uncons bs of
    Just (w8, _) | testBit w8 0 -> CIP5.addr
    _ -> CIP5.addr_test

-- | Encoding of addresses for certain key types and backend targets.
--
-- @since 2.0.0
class HasNetworkDiscriminant key => StakeAddress key where
    -- | Convert a delegation key to a stake 'Address' (aka: reward account address)
    -- valid for the given network discrimination.
    --
    -- @since 2.0.0
    stakeAddress :: NetworkDiscriminant key -> key 'DelegationK XPub -> Address

-- | Encoding of addresses for certain key types and backend targets.
--
-- @since 1.0.0
class HasNetworkDiscriminant key => PaymentAddress key where
    -- | Convert a public key to a payment 'Address' valid for the given
    -- network discrimination.
    --
    -- @since 1.0.0
    paymentAddress :: NetworkDiscriminant key -> key 'PaymentK XPub -> Address

-- | Encoding of delegation addresses for certain key types and backend targets.
--
-- @since 2.0.0
class PaymentAddress key
    => DelegationAddress key where
    -- | Convert a public key and a delegation key to a delegation 'Address' valid
    -- for the given network discrimination. Funds sent to this address will be
    -- delegated according to the delegation settings attached to the delegation
    -- key.
    --
    -- @since 2.0.0
    delegationAddress
        :: NetworkDiscriminant key
        ->  key 'PaymentK XPub
            -- ^ Payment key
        ->  key 'DelegationK XPub
            -- ^ Delegation key
        -> Address

-- | A 'ChainPointer' type representing location of some object
-- in the blockchain (eg., delegation certificate). This can be achieved
-- unambiguously by specifying slot number, transaction index and the index
-- in the object list (eg., certification list).
-- For delegation certificates, alternatively, the delegation key can be used and
-- then 'DelegationAddress' can be used.
--
-- @since 2.0.0
data ChainPointer = ChainPointer
    { slotNum :: Natural
      -- ^ Pointer to the slot
    , transactionIndex :: Natural
      -- ^ transaction index
    , outputIndex :: Natural
      -- ^ output list index
    } deriving stock (Generic, Show, Eq, Ord)
instance NFData ChainPointer

instance ToJSON ChainPointer where
    toJSON ChainPointer{..} = object
        [ "slot_num" .= slotNum
        , "transaction_index" .= transactionIndex
        , "output_index" .= outputIndex
        ]

-- | Encoding of pointer addresses for payment key type, pointer to delegation
-- certificate in the blockchain and backend targets.
--
-- @since 2.0.0
class PaymentAddress key
    => PointerAddress key where
    -- | Convert a payment public key and a pointer to delegation key in the
    -- blockchain to a delegation 'Address' valid for the given network
    -- discrimination. Funds sent to this address will be delegated according to
    -- the delegation settings attached to the delegation key located by
    -- 'ChainPointer'.
    --
    -- @since 2.0.0
    pointerAddress
        :: NetworkDiscriminant key
        ->  key 'PaymentK XPub
            -- ^ Payment key
        ->  ChainPointer
            -- ^ Pointer to locate delegation key in blockchain
        -> Address

class HasNetworkDiscriminant (key :: Depth -> Type -> Type) where
    type NetworkDiscriminant key :: Type

    addressDiscrimination :: NetworkDiscriminant key -> AddressDiscrimination
    networkTag :: NetworkDiscriminant key -> NetworkTag

-- Magic constant associated with a given network. This is mainly used in two
-- places:
--
-- (1) In 'Address' payloads, to discriminate addresses between networks.
-- (2) At the network-level, when doing handshake with nodes.
newtype NetworkTag
    = NetworkTag { unNetworkTag :: Word32 }
    deriving (Generic, Show, Eq)
instance NFData NetworkTag

instance ToJSON NetworkTag where
    toJSON (NetworkTag net) = Number (fromIntegral net)

-- Describe requirements for address discrimination on the Byron era.
data AddressDiscrimination
    = RequiresNetworkTag
    | RequiresNoTag
    deriving (Generic, Show, Eq)
instance NFData AddressDiscrimination

invariantSize :: HasCallStack => Int -> ByteString -> ByteString
invariantSize expectedLength bytes
    | BS.length bytes == expectedLength = bytes
    | otherwise = error
      $ "length was "
      ++ show (BS.length bytes)
      ++ ", but expected to be "
      ++ (show expectedLength)

invariantNetworkTag :: HasCallStack => Word32 -> NetworkTag -> Word8
invariantNetworkTag limit (NetworkTag num)
    | num < limit = fromIntegral num
    | otherwise = error
      $ "network tag was "
      ++ show num
      ++ ", but expected to be less than "
      ++ show limit
