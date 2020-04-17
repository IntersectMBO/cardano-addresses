{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Address.Derivation
    (
    -- * Overview
    -- $overview

    -- * Abstractions
      GenMasterKey (..)
    , HardDerivation (..)
    , SoftDerivation (..)

    -- * Helper types
    , Index (..)
    , Depth (..)
    , DerivationType (..)
    , AccountingStyle (..)
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv, XPub )
import Control.DeepSeq
    ( NFData )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.String
    ( fromString )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )

-- $overview
--
-- These abstractions allow generating root private key, also called /Master Key/
-- and then basing on it enable address derivation


-- | Key Depth in the derivation path, according to BIP-0039 / BIP-0044
--
-- @m | purpose' | cointype' | account' | change | address@
--
-- We do not manipulate purpose, cointype and change paths directly, so they are
-- left out of the sum type.
data Depth = RootK | AccountK | AddressK

-- | Marker for addresses type engaged. We want to handle three cases here.
-- The first two are pertinent to UTxO accounting
-- and the last one handles rewards from participation in staking.
-- (a) external chain is used for addresses that are part of the 'advertised'
--     targets of a given transaction
-- (b) internal change is for addresses used to handle the change of a
--     the transaction within a given wallet
-- (c) the addresses for a reward (chimeric) account
data AccountingStyle
    = UTxOExternal
    | UTxOInternal
    | MutableAccount
    deriving (Generic, Typeable, Show, Eq, Ord, Bounded)

instance NFData AccountingStyle

-- Not deriving 'Enum' because this could have a dramatic impact if we were
-- to assign the wrong index to the corresponding constructor (by swapping
-- around the constructor above for instance).
instance Enum AccountingStyle where
    toEnum = \case
        0 -> UTxOExternal
        1 -> UTxOInternal
        2 -> MutableAccount
        _ -> error "AccountingStyle.toEnum: bad argument"
    fromEnum = \case
        UTxOExternal -> 0
        UTxOInternal -> 1
        MutableAccount -> 2

-- | A derivation index, with phantom-types to disambiguate derivation type.
--
-- @
-- let accountIx = Index 'Hardened 'AccountK
-- let addressIx = Index 'Soft 'AddressK
-- @
newtype Index (derivationType :: DerivationType) (level :: Depth) = Index
    { getIndex :: Word32 }
    deriving stock (Generic, Show, Eq, Ord)

instance NFData (Index derivationType level)

instance Bounded (Index 'Hardened level) where
    minBound = Index 0x80000000
    maxBound = Index maxBound

instance Bounded (Index 'Soft level) where
    minBound = Index minBound
    maxBound = let (Index ix) = minBound @(Index 'Hardened _) in Index (ix - 1)

instance Bounded (Index 'WholeDomain level) where
    minBound = Index minBound
    maxBound = Index maxBound

instance Enum (Index 'Hardened level) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | Index (fromIntegral ix) < minBound @(Index 'Hardened _) =
            error "Index@Hardened.toEnum: bad argument"
        | otherwise =
            Index (fromIntegral ix)

instance Enum (Index 'Soft level) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | Index (fromIntegral ix) > maxBound @(Index 'Soft _) =
            error "Index@Soft.toEnum: bad argument"
        | otherwise =
            Index (fromIntegral ix)

instance Enum (Index 'WholeDomain level) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | Index (fromIntegral ix) > maxBound @(Index 'WholeDomain _) =
            error "Index@WholeDomain.toEnum: bad argument"
        | otherwise =
            Index (fromIntegral ix)

instance Buildable (Index derivationType level) where
    build (Index ix) = fromString (show ix)


-- | Type of derivation that should be used with the given indexes.
--
-- In theory, we should only consider two derivation types: soft and hard.
--
-- However, historically, addresses in Cardano used to be generated across the
-- both soft and hard domain. We therefore introduce a 'WholeDomain' derivation
-- type that is the exact union of `Hardened` and `Soft`.
data DerivationType = Hardened | Soft | WholeDomain

-- | An interface for doing hard derivations from the root private key, /Master Key/
class HardDerivation (key :: Depth -> * -> *) where
    type AccountIndexDerivationType key :: DerivationType
    type AddressIndexDerivationType key :: DerivationType

    -- | Derives account private key from the given root private key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    deriveAccountPrivateKey
        :: ScrubbedBytes
        -> key 'RootK XPrv
        -> Index (AccountIndexDerivationType key) 'AccountK
        -> key 'AccountK XPrv

    -- | Derives address private key from the given account private key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    deriveAddressPrivateKey
        :: ScrubbedBytes
        -> key 'AccountK XPrv
        -> AccountingStyle
        -> Index (AddressIndexDerivationType key) 'AddressK
        -> key 'AddressK XPrv

-- | An interface for doing soft derivations from an account public key
class HardDerivation key => SoftDerivation (key :: Depth -> * -> *) where
    -- | Derives address public key from the given account public key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- This is the preferred way of deriving new sequential address public keys.
    deriveAddressPublicKey
        :: key 'AccountK XPub
        -> AccountingStyle
        -> Index 'Soft 'AddressK
        -> key 'AddressK XPub


-- | Abstract interface for constructing a /Master Key/.
class GenMasterKey (key :: Depth -> * -> *) where
    type GenMasterKeyFrom key :: *

    -- | Generate a root key from a corresponding seed.
    genMasterKey :: GenMasterKeyFrom key -> ScrubbedBytes -> key 'RootK XPrv
