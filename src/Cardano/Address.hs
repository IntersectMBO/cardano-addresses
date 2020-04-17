{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Address
    ( -- * Address
      Address (..)
    , PaymentAddress (..)

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
import Data.ByteString
    ( ByteString )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

-- | Address is mere wrapper around a ByteString and represents an encoded address.
newtype Address = Address
    { unAddress :: ByteString
    } deriving (Generic, Show, Eq, Ord)
instance NFData Address

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
