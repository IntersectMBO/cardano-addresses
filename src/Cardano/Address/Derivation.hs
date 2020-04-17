{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

module Cardano.Address.Derivation
    (
    -- * Overview
    -- $overview

    -- * Key Derivation
    -- ** Types
      Index
    , Depth (..)
    , DerivationType (..)
    , AccountingStyle (..)

    -- * Abstractions
    , GenMasterKey (..)
    , HardDerivation (..)
    , SoftDerivation (..)

    -- * Low-Level Cryptography Primitives
    -- ** XPub
    , XPub
    , xpubFromBytes
    , xpubToBytes

    -- ** XPrv
    , XPrv
    , xprvFromBytes
    , xprvToBytes
    , toXPub

    -- ** XSignature
    , XSignature
    , sign
    , verify

    -- Internal / Not exposed by Haddock
    , DerivationScheme (..)
    , deriveXPrv
    , deriveXPub
    , generate
    , generateNew
    ------------------
    ) where

import Prelude

import GHC.Stack
    (HasCallStack )
import Cardano.Crypto.Wallet
    ( DerivationScheme (..) )
import Control.DeepSeq
    ( NFData )
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Data.ByteString
    ( ByteString )
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
import Crypto.Error
    ( eitherCryptoError )

import qualified Cardano.Crypto.Wallet as CC
import qualified Data.ByteString as BS
import qualified Crypto.ECC.Edwards25519 as Ed25519

-- $overview
--
-- These abstractions allow generating root private key, also called /Master Key/
-- and then basing on it enable address derivation

--
-- Low-Level Cryptography Primitives
--

-- | An opaque type representing an extended private key.
--
-- @since 1.0.0
type XPrv = CC.XPrv

-- | An opaque type representing an extended public key.
--
-- @since 1.0.0
type XPub = CC.XPub

-- | An opaque type representing a signature made from an 'XPrv'
--
-- @since 1.0.0
type XSignature = CC.XSignature

-- | Construct an 'XPub' from raw 'ByteString'
--
-- @since 1.0.0
xpubFromBytes :: ByteString -> Maybe XPub
xpubFromBytes =
    eitherToMaybe . CC.xpub

-- | Convert an 'XPub' to a raw 'ByteString'
--
-- @since 1.0.0
xpubToBytes :: XPub -> ByteString
xpubToBytes =
    CC.unXPub

-- | Construct an 'XPrv' from raw 'ByteString'.
--
-- @since 1.0.0
xprvFromBytes :: ByteString -> Maybe XPrv
xprvFromBytes bytes
    | BS.length bytes /= 96 = Nothing
    | otherwise = do
        let (prv, cc) = BS.splitAt 64 bytes
        pub <- ed25519ScalarMult (BS.take 32 prv)
        eitherToMaybe $ CC.xprv $ prv <> pub <> cc
  where
    ed25519ScalarMult :: ByteString -> Maybe ByteString
    ed25519ScalarMult bs = do
        scalar <- eitherToMaybe $ eitherCryptoError $ Ed25519.scalarDecodeLong bs
        pure $ Ed25519.pointEncode $ Ed25519.toPoint scalar

-- | Convert an 'XPrv' to a raw 'ByteString'.
--
-- @since 1.0.0
xprvToBytes :: XPrv -> ByteString
xprvToBytes =
    stripPub . CC.unXPrv
  where
    -- From  < xprv | pub | cc >
    -- ↳ To  < xprv |     | cc >
    stripPub :: ByteString -> ByteString
    stripPub xprv' = prv <> chainCode
      where
        (prv, rest) = BS.splitAt 64 xprv'
        (_pub, chainCode) = BS.splitAt 32 rest

-- | Derive the 'XPub' from a corresponding 'XPrv'.
--
-- @since 1.0.0
toXPub :: HasCallStack => XPrv -> XPub
toXPub = CC.toXPub

-- | Produce a signature of the given 'msg' from an 'XPrv'.
--
-- @since 1.0.0
sign
    :: ByteArrayAccess msg
    => XPrv
    -> msg
    -> XSignature
sign =
    CC.sign (mempty :: ScrubbedBytes)

-- | Verify the 'XSignature' of a 'msg' with the 'XPub' associated with the
-- 'XPrv' used for signing.
--
-- @since 1.0.0
verify
    :: ByteArrayAccess msg
    => XPub
    -> msg
    -> XSignature
    -> Bool
verify =
    CC.verify -- re-exported like to allow documenting it.

-- Derive a child extended private key from an extended private key
--
-- __internal__
deriveXPrv
    :: DerivationScheme
    -> XPrv
    -> Index derivationType depth
    -> XPrv
deriveXPrv ds prv (Index ix) =
    CC.deriveXPrv ds (mempty :: ScrubbedBytes) prv ix

-- Derive a child extended public key from an extended public key
--
-- __internal__
deriveXPub
    :: DerivationScheme
    -> XPub
    -> Index derivationType depth
    -> Maybe XPub
deriveXPub ds pub (Index ix) =
    CC.deriveXPub ds pub ix

-- Generate an XPrv using the legacy method (Byron).
--
-- The seed needs to be at least 32 bytes, otherwise an asynchronous error is thrown.
--
-- __internal__
generate
    :: ByteArrayAccess seed
    => seed
    -> XPrv
generate seed =
    CC.generate seed (mempty :: ScrubbedBytes)

-- Generate an XPrv using the new method (Icarus).
--
-- The seed needs to be at least 16 bytes.
--
-- __internal__
generateNew
    :: (ByteArrayAccess seed, ByteArrayAccess sndFactor)
    => seed
    -> sndFactor
    -> XPrv
generateNew seed sndFactor =
    CC.generateNew seed sndFactor (mempty :: ScrubbedBytes)

--
-- Key Derivation
--

-- | Key Depth in the derivation path, according to BIP-0039 / BIP-0044
--
-- @
-- root | purpose' | cointype' | account' | change | address@
-- 0th      1st         2nd        3rd        4th      5th
-- @
--
-- We do not manipulate purpose, cointype and change paths directly, so there
-- are no constructors for these.
--
-- @since 1.0.0
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
--
-- @since 1.0.0
newtype Index (derivationType :: DerivationType) (depth :: Depth) = Index
    { getIndex :: Word32 }
    deriving stock (Generic, Show, Eq, Ord)

instance NFData (Index derivationType depth)

instance Bounded (Index 'Hardened depth) where
    minBound = Index 0x80000000
    maxBound = Index maxBound

instance Bounded (Index 'Soft depth) where
    minBound = Index minBound
    maxBound = let (Index ix) = minBound @(Index 'Hardened _) in Index (ix - 1)

instance Bounded (Index 'WholeDomain depth) where
    minBound = Index minBound
    maxBound = Index maxBound

instance Enum (Index 'Hardened depth) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | Index (fromIntegral ix) < minBound @(Index 'Hardened _) =
            error "Index@Hardened.toEnum: bad argument"
        | otherwise =
            Index (fromIntegral ix)

instance Enum (Index 'Soft depth) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | Index (fromIntegral ix) > maxBound @(Index 'Soft _) =
            error "Index@Soft.toEnum: bad argument"
        | otherwise =
            Index (fromIntegral ix)

instance Enum (Index 'WholeDomain depth) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | Index (fromIntegral ix) > maxBound @(Index 'WholeDomain _) =
            error "Index@WholeDomain.toEnum: bad argument"
        | otherwise =
            Index (fromIntegral ix)

instance Buildable (Index derivationType depth) where
    build (Index ix) = fromString (show ix)


-- | Type of derivation that should be used with the given indexes.
--
-- In theory, we should only consider two derivation types: soft and hard.
--
-- However, historically, addresses in Cardano used to be generated across both
-- the soft and the hard domain. We therefore introduce a 'WholeDomain' derivation
-- type that is the exact union of `Hardened` and `Soft`.
--
-- @since 1.0.0
data DerivationType = Hardened | Soft | WholeDomain

-- | An interface for doing hard derivations from the root private key, /Master Key/
--
-- @since 1.0.0
class HardDerivation (key :: Depth -> * -> *) where
    type AccountIndexDerivationType key :: DerivationType
    type AddressIndexDerivationType key :: DerivationType

    -- | Derives account private key from the given root private key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- @since 1.0.0
    deriveAccountPrivateKey
        :: key 'RootK XPrv
        -> Index (AccountIndexDerivationType key) 'AccountK
        -> key 'AccountK XPrv

    -- | Derives address private key from the given account private key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- @since 1.0.0
    deriveAddressPrivateKey
        :: key 'AccountK XPrv
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
    --
    -- @since 1.0.0
    deriveAddressPublicKey
        :: key 'AccountK XPub
        -> AccountingStyle
        -> Index 'Soft 'AddressK
        -> key 'AddressK XPub


-- | Abstract interface for constructing a /Master Key/.
--
-- @since 1.0.0
class GenMasterKey (key :: Depth -> * -> *) where
    type GenMasterKeyFrom key :: *

    -- | Generate a root key from a corresponding seed.
    --
    -- @since 1.0.0
    genMasterKey :: GenMasterKeyFrom key -> key 'RootK XPrv


--
-- Internals
--

-- Simple helper to convert 'Either e' to 'Maybe' until we have a slightly
-- better approach for error.
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just
