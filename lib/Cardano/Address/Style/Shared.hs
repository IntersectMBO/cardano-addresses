{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Cardano.Address.Style.Shared
    ( -- $overview

      -- * Shared
      Shared
    , getKey
    , liftXPrv
    , liftXPub
    , sharedWalletId

      -- * Key Derivation
      -- $keyDerivation
    , genMasterKeyFromXPrv
    , genMasterKeyFromMnemonic
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveAddressPublicKey
    , deriveDelegationPrivateKey
    , deriveDelegationPublicKey
    , hashKey

    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , XPrv
    , XPub
    , hashCredential
    , hashWalletId
    , xpubPublicKey
    )
import Cardano.Address.KeyHash
    ( KeyHash (..), KeyRole )
import Cardano.Address.Script
    ( Cosigner, Script )
import Cardano.Address.Script.Parser
    ( scriptToText )
import Cardano.Address.Style.Shelley
    ( Role (..)
    , deriveAccountPrivateKeyShelley
    , deriveAddressPrivateKeyShelley
    , deriveAddressPublicKeyShelley
    , genMasterKeyFromMnemonicShelley
    )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Control.DeepSeq
    ( NFData )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Address.Derivation as Internal
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T


-- $overview
--
-- This module provides an implementation of:
--
-- - 'Cardano.Address.Derivation.GenMasterKey': for generating Shared master keys from mnemonic sentences
-- - 'Cardano.Address.Derivation.HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'Cardano.Address.Derivation.SoftDerivation': for hierarchical soft derivation of parent to child keys
--
-- - 'paymentAddress': for constructing payment addresses from a address public key or a script
-- - 'delegationAddress': for constructing delegation addresses from payment credential (public key or script) and stake credential (public key or script)
-- - 'pointerAddress': for constructing delegation addresses from payment credential (public key or script) and chain pointer
-- - 'stakeAddress': for constructing reward accounts from stake credential (public key or script)

-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types. The derivation is mostly like Shelley, except the used purpose index
-- (here 1854H rather than Shelley's 1852H)
--
-- @
-- let rootPrivateKey = Shared 'RootK XPrv
-- let accountPubKey  = Shared 'AccountK XPub
-- let addressPubKey  = Shared 'PaymentK XPub
-- @
--
-- @since 3.4.0
newtype Shared (depth :: Depth) key = Shared
    { getKey :: key
        -- ^ Extract the raw 'XPrv' or 'XPub' wrapped by this type.
        --
        -- @since 3.4.0
    }
    deriving stock (Generic, Show, Eq)

deriving instance (Functor (Shared depth))
instance (NFData key) => NFData (Shared depth key)

--
-- Key Derivation
--
-- $keyDerivation
--
-- ==== Generating a root key from 'SomeMnemonic'
--
-- @
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Cardano.Mnemonic ( mkSomeMnemonic )
-- @
--
-- @
-- >>> let (Right mw) = mkSomeMnemonic @'[15] ["network","empty","cause","mean","expire","private","finger","accident","session","problem","absurd","banner","stage","void","what"]
-- >>> let sndFactor = mempty -- Or alternatively, a second factor mnemonic transformed to bytes via someMnemonicToBytes
-- >>> let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shared 'RootK XPrv
-- @
--
-- ==== Deriving child keys
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'\/0\/14@
--
-- @
-- >>> let Just accIx = indexFromWord32 0x80000000
-- >>> let acctK = deriveAccountPrivateKey rootK accIx
-- >>>
-- >>> let Just addIx = indexFromWord32 0x00000014
-- >>> let addrK = deriveAddressPrivateKey acctK UTxOExternal addIx
--
-- >>> let stakeK = deriveDelegationPrivateKey acctK
-- @
instance Internal.GenMasterKey Shared where
    type SecondFactor Shared = ScrubbedBytes

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic fstFactor sndFactor =
        Shared $ genMasterKeyFromMnemonicShelley fstFactor sndFactor

instance Internal.HardDerivation Shared where
    type AccountIndexDerivationType Shared = 'Hardened
    type AddressIndexDerivationType Shared = 'Soft
    type WithRole Shared = Role

    deriveAccountPrivateKey (Shared rootXPrv) accIx =
        Shared $ deriveAccountPrivateKeyShelley rootXPrv accIx purposeIndex

    deriveAddressPrivateKey (Shared accXPrv) keyRole addrIx =
        Shared $ deriveAddressPrivateKeyShelley accXPrv keyRole addrIx

instance Internal.SoftDerivation Shared where
    deriveAddressPublicKey (Shared accXPub) keyRole addrIx =
        Shared $ deriveAddressPublicKeyShelley accXPub keyRole addrIx

-- | Generate a root key from a corresponding mnemonic.
--
-- @since 3.4.0
genMasterKeyFromMnemonic
    :: SomeMnemonic
        -- ^ Some valid mnemonic sentence.
    -> ScrubbedBytes
        -- ^ An optional second-factor passphrase (or 'mempty')
    -> Shared 'RootK XPrv
genMasterKeyFromMnemonic = Internal.genMasterKeyFromMnemonic

-- | Generate a root key from a corresponding root 'XPrv'
--
-- @since 3.4.0
genMasterKeyFromXPrv :: XPrv -> Shared 'RootK XPrv
genMasterKeyFromXPrv = Internal.genMasterKeyFromXPrv

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an account private key from the given root private key.
--
-- @since 3.4.0
deriveAccountPrivateKey
    :: Shared 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> Shared 'AccountK XPrv
deriveAccountPrivateKey = Internal.deriveAccountPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives a multisig private key from the given account private key for payment credential.
--
-- @since 3.4.0
deriveAddressPrivateKey
    :: Shared 'AccountK XPrv
    -> Role
    -> Index 'Soft 'PaymentK
    -> Shared 'ScriptK XPrv
deriveAddressPrivateKey = coerce . Internal.deriveAddressPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives a multisig private key from the given account private key for delegation credential.
--
-- @since 3.4.0
deriveDelegationPrivateKey
    :: Shared 'AccountK XPrv
    -> Index 'Soft 'PaymentK
    -> Shared 'ScriptK XPrv
deriveDelegationPrivateKey accPrv = coerce .
    Internal.deriveAddressPrivateKey accPrv Stake

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derives a multisig public key from the given account public key for payment credential.
--
-- @since 3.4.0
deriveAddressPublicKey
    :: Shared 'AccountK XPub
    -> Role
    -> Index 'Soft 'PaymentK
    -> Shared 'ScriptK XPub
deriveAddressPublicKey = coerce . Internal.deriveAddressPublicKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derives a multisig public key from the given account public key for delegation credential.
--
-- @since 3.4.0
deriveDelegationPublicKey
    :: Shared 'AccountK XPub
    -> Index 'Soft 'PaymentK
    -> Shared 'ScriptK XPub
deriveDelegationPublicKey accPub = coerce .
    Internal.deriveAddressPublicKey accPub Stake

--
-- Unsafe
--

-- | Unsafe backdoor for constructing an 'Shared' key from a raw 'XPrv'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 3.4.0
liftXPrv :: XPrv -> Shared depth XPrv
liftXPrv = Shared

-- | Unsafe backdoor for constructing an 'Shared' key from a raw 'XPub'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 3.4.0
liftXPub :: XPub -> Shared depth XPub
liftXPub = Shared


-- | Calculates wallet id of shared wallet
-- It takes raw bytes of account public kye (64-bytes),
-- spending script template, and
-- optionally staking script template.
--
-- @since 3.10.0
sharedWalletId
    :: ByteString
    -> Script Cosigner
    -> Maybe (Script Cosigner)
    -> ByteString
sharedWalletId bytes spending stakingM =
    if BS.length bytes == 64 then
        hashWalletId $
        bytes <>
        serializeScriptTemplate spending <>
        maybe mempty serializeScriptTemplate stakingM
    else
        error "Extended account public key is expected to have 64 bytes."
  where
    serializeScriptTemplate = T.encodeUtf8 . scriptToText

--
-- Internal
--

--- | Computes a 28-byte Blake2b224 digest of a Shared 'XPub'.
---
--- @since 3.4.0
hashKey :: KeyRole -> Shared key XPub -> KeyHash
hashKey cred = KeyHash cred . hashCredential . xpubPublicKey . getKey

-- | Purpose is a constant set to 1854' (or 0x8000073e) following the
-- CIP-1854 Multi-signatures HD Wallets
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000073e
