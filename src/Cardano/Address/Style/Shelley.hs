{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0

module Cardano.Address.Style.Shelley
    ( -- $overview

      -- * Shelley
      Shelley

      -- * Accessors
    , getKey

      -- * Unsafe
    , liftXPrv

      -- Internals
    , minSeedLengthBytes
    , publicKeySize
    , addrSingleSize
    , addrGroupedSize
    ) where

import Prelude

import Cardano.Address.Derivation
    ( AccountingStyle
    , Depth (..)
    , DerivationScheme (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index
    , SoftDerivation (..)
    , XPrv
    , deriveXPrv
    , deriveXPub
    , generateNew
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy )
import Control.DeepSeq
    ( NFData )
import Control.Exception.Base
    ( assert )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Data.ByteArray as BA

-- $overview
--
-- This module provides an implementation of:
--
-- - 'GenMasterKey': for generating Shelley master keys from mnemonic sentences
-- - 'HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'SoftDerivation': for hierarchical soft derivation of parent to child keys
--

{-------------------------------------------------------------------------------
                                   Key Types
-------------------------------------------------------------------------------}
-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Shelley 'RootK XPrv
-- let accountPubKey  = Shelley 'AccountK XPub
-- let addressPubKey  = Shelley 'AddressK XPub
-- @
--
-- @since 1.0.0
newtype Shelley (depth :: Depth) key = Shelley
    { getKey :: key
        -- ^ Extract the raw 'XPrv' or 'XPub' wrapped by this type.
        --
        -- @since 1.0.0
    }
    deriving stock (Generic, Show, Eq)

deriving instance (Functor (Shelley depth))
instance (NFData key) => NFData (Shelley depth key)

-- | Unsafe backdoor for constructing an 'Shelley' key from a raw 'XPrv'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 1.0.0
liftXPrv :: XPrv -> Shelley depth XPrv
liftXPrv = Shelley

instance GenMasterKey Shelley where
    type SecondFactor Shelley = Maybe SomeMnemonic

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic fstFactor sndFactor =
        Shelley $ generateNew seedValidated (maybe mempty mnemonicToBytes sndFactor)
        where
            mnemonicToBytes (SomeMnemonic mw) = entropyToBytes $ mnemonicToEntropy mw
            seed  = mnemonicToBytes fstFactor
            seedValidated = assert
                (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
                seed

instance HardDerivation Shelley where
    type AccountIndexDerivationType Shelley = 'Hardened
    type AddressIndexDerivationType Shelley = 'Soft
    type WithAccountStyle Shelley = AccountingStyle

    deriveAccountPrivateKey (Shelley rootXPrv) accIx =
        let
            purposeIx =
                toEnum @(Index 'Hardened _) $ fromEnum purposeIndex
            coinTypeIx =
                toEnum @(Index 'Hardened _) $ fromEnum coinTypeIndex
            purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
                deriveXPrv DerivationScheme2 rootXPrv purposeIx
            coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
                deriveXPrv DerivationScheme2 purposeXPrv coinTypeIx
            acctXPrv = -- lvl3 derivation; hardened derivation of account' index
                deriveXPrv DerivationScheme2 coinTypeXPrv accIx
        in
            Shelley acctXPrv

    deriveAddressPrivateKey (Shelley accXPrv) accountingStyle addrIx =
        let
            changeCode =
                toEnum @(Index 'Soft _) $ fromEnum accountingStyle
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv changeCode
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv addrIx
        in
            Shelley addrXPrv

instance SoftDerivation Shelley where
    deriveAddressPublicKey (Shelley accXPub) accountingStyle addrIx =
        fromMaybe errWrongIndex $ do
            let changeCode = toEnum @(Index 'Soft _) $ fromEnum accountingStyle
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub changeCode
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ Shelley addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

{-------------------------------------------------------------------------------
                                 Key generation
-------------------------------------------------------------------------------}

-- | Size, in bytes, of a public key (without chain code)
publicKeySize :: Int
publicKeySize = 32

-- Serialized length in bytes of a Single Address
addrSingleSize :: Int
addrSingleSize = 1 + publicKeySize

-- Serialized length in bytes of a Grouped Address
addrGroupedSize :: Int
addrGroupedSize = addrSingleSize + publicKeySize

-- | Purpose is a constant set to 1852' (or 0x8000073c) following the BIP-44
-- extension for Cardano:
--
-- https://github.com/input-output-hk/implementation-decisions/blob/e2d1bed5e617f0907bc5e12cf1c3f3302a4a7c42/text/1852-hd-chimeric.md
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000073c

-- | One master node (seed) can be used for unlimited number of independent
-- cryptocoins such as Bitcoin, Litecoin or Namecoin. However, sharing the
-- same space for various cryptocoins has some disadvantages.
--
-- This level creates a separate subtree for every cryptocoin, avoiding reusing
-- addresses across cryptocoins and improving privacy issues.
--
-- Coin type is a constant, set for each cryptocoin. For Cardano this constant
-- is set to 1815' (or 0x80000717). 1815 is the birthyear of our beloved Ada
-- Lovelace.
--
-- Hardened derivation is used at this level.
coinTypeIndex :: Word32
coinTypeIndex = 0x80000717

-- | The minimum seed length for 'genMasterKeyFromMnemonic'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16
