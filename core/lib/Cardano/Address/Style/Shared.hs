{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0

module Cardano.Address.Style.Shared
    ( -- $overview

      -- * Shared
      Shared
    , getKey
    , liftXPrv
    , liftXPub

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
    , DerivationScheme (..)
    , DerivationType (..)
    , Index (..)
    , XPrv
    , XPub
    , deriveXPrv
    , deriveXPub
    , generateNew
    , hashCredential
    , indexFromWord32
    , xpubPublicKey
    )
import Cardano.Address.Script
    ( KeyHash (..), KeyType )
import Cardano.Address.Style.Shelley
    ( Role (..), coinTypeIndex, minSeedLengthBytes, roleToIndex )
import Cardano.Mnemonic
    ( SomeMnemonic, someMnemonicToBytes )
import Control.DeepSeq
    ( NFData )
import Control.Exception.Base
    ( assert )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.Coerce
    ( coerce )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Address.Derivation as Internal
import qualified Data.ByteArray as BA

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
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Shared 'RootK XPrv
-- let accountPubKey  = Shared 'AccountK XPub
-- let addressPubKey  = Shared 'PaymentK XPub
-- @
--
-- @since 3.3.0
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
-- === Generating a root key from 'SomeMnemonic'
-- > :set -XOverloadedStrings
-- > :set -XTypeApplications
-- > :set -XDataKinds
-- > import Cardano.Mnemonic ( mkSomeMnemonic )
-- >
-- > let (Right mw) = mkSomeMnemonic @'[15] ["network","empty","cause","mean","expire","private","finger","accident","session","problem","absurd","banner","stage","void","what"]
-- > let sndFactor = mempty -- Or alternatively, a second factor mnemonic transformed to bytes via someMnemonicToBytes
-- > let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shared 'RootK XPrv
--
-- === Deriving child keys
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'\/0\/14@
--
-- > let Just accIx = indexFromWord32 0x80000000
-- > let acctK = deriveAccountPrivateKey rootK accIx
-- >
-- > let Just addIx = indexFromWord32 0x00000014
-- > let addrK = deriveAddressPrivateKey acctK UTxOExternal addIx
--
-- > let stakeK = deriveDelegationPrivateKey acctK

instance Internal.GenMasterKey Shared where
    type SecondFactor Shared = ScrubbedBytes

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic fstFactor sndFactor =
        Shared $ generateNew seedValidated sndFactor
        where
            seed  = someMnemonicToBytes fstFactor
            seedValidated = assert
                (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
                seed

instance Internal.HardDerivation Shared where
    type AccountIndexDerivationType Shared = 'Hardened
    type AddressIndexDerivationType Shared = 'Soft
    type WithRole Shared = Role

    deriveAccountPrivateKey (Shared rootXPrv) accIx =
        let
            Just purposeIx =
                indexFromWord32 @(Index 'Hardened _) purposeIndex
            Just coinTypeIx =
                indexFromWord32 @(Index 'Hardened _) coinTypeIndex
            purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
                deriveXPrv DerivationScheme2 rootXPrv purposeIx
            coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
                deriveXPrv DerivationScheme2 purposeXPrv coinTypeIx
            acctXPrv = -- lvl3 derivation; hardened derivation of account' index
                deriveXPrv DerivationScheme2 coinTypeXPrv accIx
        in
            Shared acctXPrv

    deriveAddressPrivateKey (Shared accXPrv) role addrIx =
        let
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv (roleToIndex role)
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv addrIx
        in
            Shared addrXPrv

instance Internal.SoftDerivation Shared where
    deriveAddressPublicKey (Shared accXPub) role addrIx =
        fromMaybe errWrongIndex $ do
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub (roleToIndex role)
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ Shared addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

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
    -> Index 'Soft 'PaymentK
    -> Shared 'ScriptK XPrv
deriveAddressPrivateKey accPrv = coerce .
    Internal.deriveAddressPrivateKey accPrv UTxOExternal

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
    -> Index 'Soft 'PaymentK
    -> Shared 'ScriptK XPub
deriveAddressPublicKey accPub = coerce .
    Internal.deriveAddressPublicKey accPub UTxOExternal

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

--
-- Internal
--

--- | Computes a 28-byte Blake2b224 digest of a Shared 'XPub'.
---
--- @since 3.4.0
hashKey :: KeyType -> Shared key XPub -> KeyHash
hashKey cred = flip KeyHash cred . hashCredential . xpubPublicKey . getKey

-- Purpose is a constant set to 1854' (or 0x8000073e) following the
-- CIP-1854 Multi-signatures HD Wallets
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000073e
