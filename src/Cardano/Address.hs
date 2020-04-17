{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Address
    ( -- * Address
      Address (..)
    , PaymentAddress (..)

      -- * Network Discrimination
    , NetworkDiscriminant (..)
    , ProtocolMagic (..)
    , mainnetMagic
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
import Data.Proxy
    ( Proxy (..) )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )

-- | Address is mere wrapper around a ByteString and represents an encoded address.
newtype Address = Address
    { unAddress :: ByteString
    } deriving (Show, Generic, Eq, Ord)

-- | Encoding of addresses for certain key types and backend targets.
class PaymentAddress (network :: NetworkDiscriminant) key where
    -- | Convert a public key to a payment 'Address' valid for the given
    -- network discrimination.
    --
    -- Note that 'paymentAddress' is ambiguous and requires therefore a type
    -- application.
    paymentAddress :: key 'AddressK XPub -> Address

instance NFData Address

-- | Available network options.
data NetworkDiscriminant = Mainnet | Testnet Nat

-- | Magic constant associated to a given network
newtype ProtocolMagic = ProtocolMagic { getProtocolMagic :: Word32 }
    deriving (Generic, Show, Eq)

-- | Hard-coded protocol magic for the Byron MainNet
mainnetMagic :: ProtocolMagic
mainnetMagic =  ProtocolMagic 764824073

-- | Derive testnet magic from a type-level Nat
testnetMagic :: forall pm. KnownNat pm => ProtocolMagic
testnetMagic = ProtocolMagic $ fromIntegral $ natVal $ Proxy @pm
