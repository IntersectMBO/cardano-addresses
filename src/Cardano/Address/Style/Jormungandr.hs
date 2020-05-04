{-# LANGUAGE AllowAmbiguousTypes #-}
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

module Cardano.Address.Style.Jormungandr
    ( -- $overview

      -- * Jormungandr
      Jormungandr

      -- * Accessors
    , getKey

      -- * Discrimination
    , jormungandrTestnet

      -- * Unsafe
    , liftXPrv

      -- Internals
    -- * Constants
    , minSeedLengthBytes
    , publicKeySize
    , addrSingleSize
    , addrGroupedSize
    ) where

import Prelude

import Cardano.Address
    ( AddressDiscrimination (..)
    , DelegationAddress (..)
    , HasNetworkDiscriminant (..)
    , NetworkTag (..)
    , PaymentAddress (..)
    , invariantSize
    , unsafeMkAddress
    )
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
    , getPublicKey
    )
import Cardano.Mnemonic
    ( someMnemonicToBytes )
import Control.DeepSeq
    ( NFData )
import Control.Exception.Base
    ( assert )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BL

-- $overview
--
-- This module provides an implementation of:
--
-- - 'GenMasterKey': for generating Jormungandr master keys from mnemonic sentences
-- - 'HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'SoftDerivation': for hierarchical soft derivation of parent to child keys
-- - 'PaymentAddress': for constructing payment addresses from a address public key
--
-- == Examples
--
-- === Generating a root key from 'SomeMnemonic'
-- > :set -XOverloadedStrings
-- > :set -XTypeApplications
-- > :set -XDataKinds
-- > import Cardano.Mnemonic ( mkSomeMnemonic )
-- > import Cardano.Address.Derivation ( GenMasterKey(..) )
-- >
-- > let (Right mw) = mkSomeMnemonic @'[15] ["network","empty","cause","mean","expire","private","finger","accident","session","problem","absurd","banner","stage","void","what"]
-- > let sndFactor = mempty -- Or alternatively, a second factor mnemonic transformed to bytes via someMnemonicToBytes
-- > let rootK = genMasterKeyFromMnemonic mw sndFactor :: Jormungandr 'RootK XPrv
--
-- === Generating an 'Address' from a root key
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'/0/14@
--
--
-- > import Cardano.Address ( PaymentAddress(..), bech32 )
-- > import Cardano.Address.Derivation ( AccountingStyle(..), GenMasterKey(..), toXPub )
-- >
-- > let accIx = toEnum 0x80000000
-- > let acctK = deriveAccountPrivateKey rootK accIx
-- >
-- > let addIx = toEnum 0x00000014
-- > let addrK = deriveAddressPrivateKey acctK UTxOExternal addIx
-- >
-- > bech32 $ paymentAddress jormungandrTestnet (toXPub <$> addrK)
-- > "addr1s0lgjsr0kjsprvkxmetgcjaxsq833rxg3g8rv528wa0l5c8wcnplq3x0w2h"

{-------------------------------------------------------------------------------
                                   Key Types
-------------------------------------------------------------------------------}
-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Jormungandr 'RootK XPrv
-- let accountPubKey  = Jormungandr 'AccountK XPub
-- let addressPubKey  = Jormungandr 'AddressK XPub
-- @
--
-- @since 2.0.0
newtype Jormungandr (depth :: Depth) key = Jormungandr
    { getKey :: key
        -- ^ Extract the raw 'XPrv' or 'XPub' wrapped by this type.
        --
        -- @since 2.0.0
    }
    deriving stock (Generic, Show, Eq)

deriving instance (Functor (Jormungandr depth))
instance (NFData key) => NFData (Jormungandr depth key)

-- | Unsafe backdoor for constructing an 'Jormungandr' key from a raw 'XPrv'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 2.0.0
liftXPrv :: XPrv -> Jormungandr depth XPrv
liftXPrv = Jormungandr

instance GenMasterKey Jormungandr where
    type SecondFactor Jormungandr = ScrubbedBytes

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic fstFactor sndFactor =
        Jormungandr $ generateNew seedValidated sndFactor
        where
            seed  = someMnemonicToBytes fstFactor
            seedValidated = assert
                (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
                seed

instance HardDerivation Jormungandr where
    type AccountIndexDerivationType Jormungandr = 'Hardened
    type AddressIndexDerivationType Jormungandr = 'Soft
    type WithAccountStyle Jormungandr = AccountingStyle

    deriveAccountPrivateKey (Jormungandr rootXPrv) accIx =
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
            Jormungandr acctXPrv

    deriveAddressPrivateKey (Jormungandr accXPrv) accountingStyle addrIx =
        let
            changeCode =
                toEnum @(Index 'Soft _) $ fromEnum accountingStyle
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv changeCode
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv addrIx
        in
            Jormungandr addrXPrv

instance SoftDerivation Jormungandr where
    deriveAddressPublicKey (Jormungandr accXPub) accountingStyle addrIx =
        fromMaybe errWrongIndex $ do
            let changeCode = toEnum @(Index 'Soft _) $ fromEnum accountingStyle
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub changeCode
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ Jormungandr addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

instance HasNetworkDiscriminant Jormungandr where
    type NetworkDiscriminant Jormungandr = (AddressDiscrimination, NetworkTag)
    addressDiscrimination = fst
    networkTag = snd

instance PaymentAddress Jormungandr where
    paymentAddress discrimination k = unsafeMkAddress $
        invariantSize expectedLength $ BL.toStrict $ runPut $ do
            putWord8 firstByte
            putByteString (getPublicKey $ getKey k)
      where
          firstByte = case addressDiscrimination @Jormungandr discrimination of
              RequiresNetworkTag -> 0x83
              RequiresNoTag -> 0x03
          expectedLength = 1 + publicKeySize

instance DelegationAddress Jormungandr where
    delegationAddress discrimination paymentKey stakingKey = unsafeMkAddress $
        invariantSize expectedLength $ BL.toStrict $ runPut $ do
            putWord8 firstByte
            putByteString . getPublicKey . getKey $ paymentKey
            putByteString . getPublicKey . getKey $ stakingKey
      where
          firstByte = case addressDiscrimination @Jormungandr discrimination of
              RequiresNetworkTag -> 0x84
              RequiresNoTag -> 0x04
          expectedLength = 1 + 2*publicKeySize

    deriveStakingPrivateKey (Jormungandr accXPrv) =
        let
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv (toEnum @(Index 'Soft _) 2)
            stakeXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv (minBound @(Index 'Soft _))
        in
            Jormungandr stakeXPrv

-- | 'NetworkDiscriminant' for Cardano TestNet & Jormungandr
--
-- @since 2.0.0
jormungandrTestnet :: NetworkDiscriminant Jormungandr
jormungandrTestnet = (RequiresNetworkTag, NetworkTag 0)

{-------------------------------------------------------------------------------
                                 Key generation
-------------------------------------------------------------------------------}

-- | Size, in bytes, of a public key (without chain code)
publicKeySize :: Int
publicKeySize = 32

-- | Serialized length in bytes of a Single Address
addrSingleSize :: Int
addrSingleSize = 1 + publicKeySize

-- | Serialized length in bytes of a Grouped Address
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

-- | The minimum seed length for 'generateKeyFromSeed' and
-- 'unsafeGenerateKeyFromSeed'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16
