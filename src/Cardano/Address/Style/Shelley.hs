{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , getKey

      -- * Key Derivation
      -- $keyDerivation
    , genMasterKeyFromXPrv
    , genMasterKeyFromMnemonic
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveStakingPrivateKey
    , deriveAddressPublicKey

      -- * Addresses
      -- $addresses
    , paymentAddress
    , delegationAddress
    , pointerAddress

      -- * Network Discrimination
    , MkNetworkDiscriminantError (..)
    , mkNetworkDiscriminant

      -- * Unsafe
    , liftXPrv
    , liftXPub

      -- Internals
    , minSeedLengthBytes
    , publicKeyHashSize
    ) where

import Prelude

import Cardano.Address
    ( Address
    , AddressDiscrimination (..)
    , ChainPointer (..)
    , NetworkDiscriminant (..)
    , NetworkTag (..)
    , invariantNetworkTag
    , invariantSize
    , unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( AccountingStyle
    , Depth (..)
    , DerivationScheme (..)
    , DerivationType (..)
    , Index
    , XPrv
    , XPub
    , deriveXPrv
    , deriveXPub
    , generateNew
    , xpubPublicKey
    )
import Cardano.Mnemonic
    ( SomeMnemonic, someMnemonicToBytes )
import Control.DeepSeq
    ( NFData )
import Control.Exception.Base
    ( assert )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_224 (..) )
import Crypto.Hash.IO
    ( HashAlgorithm (hashDigestSize) )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word32, Word8 )
import Data.Word7
    ( limit, putVariableLengthNat )
import GHC.Generics
    ( Generic )

import qualified Cardano.Address as Internal
import qualified Cardano.Address.Derivation as Internal
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BL

-- $overview
--
-- This module provides an implementation of:
--
-- - 'Cardano.Address.Derivation.GenMasterKey': for generating Shelley master keys from mnemonic sentences
-- - 'Cardano.Address.Derivation.HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'Cardano.Address.Derivation.SoftDerivation': for hierarchical soft derivation of parent to child keys
-- - 'Cardano.Address.PaymentAddress': for constructing payment addresses from a address public key
-- - 'Cardano.Address.DelegationAddress': for constructing delegation addresses from address and staking public keys

-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Shelley 'RootK XPrv
-- let accountPubKey  = Shelley 'AccountK XPub
-- let addressPubKey  = Shelley 'AddressK XPub
-- @
--
-- @since 2.0.0
newtype Shelley (depth :: Depth) key = Shelley
    { getKey :: key
        -- ^ Extract the raw 'XPrv' or 'XPub' wrapped by this type.
        --
        -- @since 1.0.0
    }
    deriving stock (Generic, Show, Eq)

deriving instance (Functor (Shelley depth))
instance (NFData key) => NFData (Shelley depth key)

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
-- > let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
--
-- === Deriving child keys
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'\/0\/14@
--
-- > import Cardano.Address.Derivation ( AccountingStyle(..) )
-- >
-- > let accIx = toEnum 0x80000000
-- > let acctK = deriveAccountPrivateKey rootK accIx
-- >
-- > let addIx = toEnum 0x00000014
-- > let addrK = deriveAddressPrivateKey acctK UTxOExternal addIx
--
-- > let stakeK = deriveStakingPrivateKey acctK

instance Internal.GenMasterKey Shelley where
    type SecondFactor Shelley = ScrubbedBytes

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic fstFactor sndFactor =
        Shelley $ generateNew seedValidated sndFactor
        where
            seed  = someMnemonicToBytes fstFactor
            seedValidated = assert
                (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
                seed

instance Internal.HardDerivation Shelley where
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

instance Internal.SoftDerivation Shelley where
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

instance Internal.StakingDerivation Shelley where
    deriveStakingPrivateKey (Shelley accXPrv) =
        let
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv (toEnum @(Index 'Soft _) 2)
            stakeXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv (minBound @(Index 'Soft _))
        in
            Shelley stakeXPrv

-- | Generate a root key from a corresponding mnemonic.
--
-- @since 2.0.0
genMasterKeyFromMnemonic
    :: SomeMnemonic
        -- ^ Some valid mnemonic sentence.
    -> ScrubbedBytes
        -- ^ An optional second-factor passphrase (or 'mempty')
    -> Shelley 'RootK XPrv
genMasterKeyFromMnemonic =
    Internal.genMasterKeyFromMnemonic

-- | Generate a root key from a corresponding root 'XPrv'
--
-- @since 2.0.0
genMasterKeyFromXPrv
    :: XPrv -> Shelley 'RootK XPrv
genMasterKeyFromXPrv =
    Internal.genMasterKeyFromXPrv

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an account private key from the given root private key.
--
-- @since 2.0.0
deriveAccountPrivateKey
    :: Shelley 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> Shelley 'AccountK XPrv
deriveAccountPrivateKey =
    Internal.deriveAccountPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an address private key from the given account private key.
--
-- @since 2.0.0
deriveAddressPrivateKey
    :: Shelley 'AccountK XPrv
    -> AccountingStyle
    -> Index 'Soft 'AddressK
    -> Shelley 'AddressK XPrv
deriveAddressPrivateKey =
    Internal.deriveAddressPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derives an address public key from the given account public key.
--
-- @since 2.0.0
deriveAddressPublicKey
    :: Shelley 'AccountK XPub
    -> AccountingStyle
    -> Index 'Soft 'AddressK
    -> Shelley 'AddressK XPub
deriveAddressPublicKey =
    Internal.deriveAddressPublicKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derive a staking key for a corresponding 'AccountK'. Note that wallet
-- software are by convention only using one staking key per account, and always
-- the first account (with index 0').
--
-- Deriving staking keys for something else than the initial account is not
-- recommended and can lead to incompatibility with existing wallet softwares
-- (Daedalus, Yoroi, Adalite...).
--
-- @since 2.0.0
deriveStakingPrivateKey
    :: Shelley 'AccountK XPrv
    -> Shelley 'StakingK XPrv
deriveStakingPrivateKey =
    Internal.deriveStakingPrivateKey

--
-- Addresses
--
-- $addresses
-- === Generating a 'PaymentAddress'
--
-- > import Cardano.Address ( bech32 )
-- > import Cardano.Address.Derivation ( AccountingStyle(..), toXPub )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > bech32 $ paymentAddress tag (toXPub <$> addrK)
-- > "addr1vxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdncxsce5t"
--
-- === Generating a 'DelegationAddress'
--
-- > import Cardano.Address ( DelegationAddress (..) )
-- > import Cardano.Address.Derivation ( StakingDerivation (..) )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > bech32 $ delegationAddress tag (toXPub <$> addrK) (toXPub <$> stakeK)
-- > "addr1qxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdn7nudck0fzve4346yytz3wpwv9yhlxt7jwuc7ytwx2vfkyqmkc5xa"
--
-- === Generating a 'PointerAddress'
--
-- > import Cardano.Address ( PointerAddress (..), ChainPointer (..) )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > let ptr = ChainPointer 123 1 2
-- > bech32 $ pointerAddress tag (toXPub <$> addrK) ptr
-- > "addr1gxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdnmmqypqfcp5um"

instance Internal.PaymentAddress Shelley where
    paymentAddress discrimination k = unsafeMkAddress $
        invariantSize expectedLength $ BL.toStrict $ runPut $ do
            putWord8 firstByte
            putByteString (blake2b224 k)
      where
          -- we use here the fact that payment address stands for what is named
          -- as enterprise address, ie., address carrying no stake rights. For
          -- rationale why we may need such addresses refer to delegation
          -- specification - Section 3.2.3. What is important here is that the
          -- address is composed of discrimination byte and 28 bytes hashed public key.
          -- Moreover, it was decided that first 4 bits for enterprise address
          -- will be `0110`. The next for 4 bits are reserved for network discriminator.
          -- `0110 0000` is 96 in decimal.
          firstByte =
              96 + invariantNetworkTag 16 (networkTag @Shelley discrimination)
          expectedLength = 1 + publicKeyHashSize

instance Internal.DelegationAddress Shelley where
    delegationAddress discrimination paymentKey stakingKey = unsafeMkAddress $
        invariantSize expectedLength $ BL.toStrict $ runPut $ do
            putWord8 firstByte
            putByteString . blake2b224 $ paymentKey
            putByteString . blake2b224 $ stakingKey
      where
          -- we use here the fact that delegation address stands for what is named
          -- as base address - refer to delegation specification - Section 3.2.1.
          -- What is important here is that the address is composed of discrimination
          -- byte and two 28 bytes hashed public keys, one for payment key and the
          -- other for staking/reword key.
          -- Moreover, it was decided that first 4 bits for enterprise address
          -- will be `0000`. The next for bits are reserved for network discriminator.
          firstByte =
              invariantNetworkTag 16 (networkTag @Shelley discrimination)
          expectedLength = 1 + 2*publicKeyHashSize

instance Internal.PointerAddress Shelley where
    pointerAddress discrimination key ptr@(ChainPointer sl ix1 ix2) =
        unsafeMkAddress $
        invariantSize expectedLength $ BL.toStrict $ runPut $ do
            putWord8 firstByte
            putByteString (blake2b224 key)
            putPointer ptr
      where
          -- we use here the fact that pointer address stands for what is named
          -- the same in delegation specification - Section 3.2.4. What is
          -- important here is that the address is composed of discrimination
          -- byte, 28 bytes hashed public key and three numbers depicting slot
          -- and indices. Moreover, it was decided that first 4 bits for pointer
          -- address will be `0100`. The next for 4 bits are reserved for network
          -- discriminator. `0100 0000` is 64 in decimal.
          firstByte =
              64 + invariantNetworkTag 16 (networkTag @Shelley discrimination)
          expectedLength = 1 + publicKeyHashSize + pointerLength

          pointerLength =
                    sum $ calculateLength <$>
                    [sl, fromIntegral ix1, fromIntegral ix2]

          calculateLength inp
              | inp <= fromIntegral (limit 7) = 1
              | inp <= fromIntegral (limit 14) = 2
              | inp <= fromIntegral (limit 21) = 3
              | inp <= fromIntegral (limit 28) = 4
              | inp <= fromIntegral (limit 35) = 5
              | inp <= fromIntegral (limit 42) = 6
              | inp <= fromIntegral (limit 49) = 7
              | inp <= fromIntegral (limit 56) = 8
              | inp <= fromIntegral (limit 63) = 9
              | otherwise = 10


          putPointer (ChainPointer slotN ix1' ix2') = do
              putVariableLengthNat (fromIntegral slotN)
              putVariableLengthNat ix1'
              putVariableLengthNat ix2'

-- Re-export from 'Cardano.Address' to have it documented specialized in Haddock.
--
-- | Convert a public key to a payment 'Address' valid for the given
-- network discrimination.
--
-- @since 2.0.0
paymentAddress
    :: NetworkDiscriminant Shelley
    -> Shelley 'AddressK XPub
    -> Address
paymentAddress =
    Internal.paymentAddress

-- Re-export from 'Cardano.Address' to have it documented specialized in Haddock.
--
-- | Convert a public key and a staking key to a delegation 'Address' valid
-- for the given network discrimination. Funds sent to this address will be
-- delegated according to the delegation settings attached to the delegation
-- key.
--
-- @since 2.0.0
delegationAddress
    :: NetworkDiscriminant Shelley
    -> Shelley 'AddressK XPub
    -> Shelley 'StakingK XPub
    -> Address
delegationAddress =
    Internal.delegationAddress


-- Re-export from 'Cardano.Address' to have it documented specialized in Haddock.
--
-- | Convert a public key and pointer to staking certificate in blockchain to a
-- pointer 'Address' valid for the given network discrimination.
--
-- @since 2.0.0
pointerAddress
    :: NetworkDiscriminant Shelley
    -> Shelley 'AddressK XPub
    -> ChainPointer
    -> Address
pointerAddress =
    Internal.pointerAddress

--
-- Network Discriminant
--

instance HasNetworkDiscriminant Shelley where
    type NetworkDiscriminant Shelley = NetworkTag
    addressDiscrimination _ = RequiresNetworkTag
    networkTag = id

-- | Error reported from trying to create a network discriminant from number
--
-- @since 2.0.0
newtype MkNetworkDiscriminantError
    = ErrWrongNetworkTag Word8
      -- ^ Wrong network tag.
    deriving (Eq, Show)

-- | Construct 'NetworkDiscriminant' for Cardano 'Shelley' from a number.
-- If the number is invalid, ie., not between 0 and 15, then
-- 'MkNetworkDiscriminantError' is thrown.
--
-- @since 2.0.0
mkNetworkDiscriminant
    :: Word8
    -> Either MkNetworkDiscriminantError (NetworkDiscriminant Shelley)
mkNetworkDiscriminant nTag
    | nTag < 16 =  Right $ NetworkTag $ fromIntegral nTag
    | otherwise = Left $ ErrWrongNetworkTag nTag

--
-- Unsafe
--

-- | Unsafe backdoor for constructing an 'Shelley' key from a raw 'XPrv'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 2.0.0
liftXPrv :: XPrv -> Shelley depth XPrv
liftXPrv = Shelley

-- | Unsafe backdoor for constructing an 'Shelley' key from a raw 'XPub'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 2.0.0
liftXPub :: XPub -> Shelley depth XPub
liftXPub = Shelley

--
-- Internal
--

-- Hash a public key
blake2b224 :: Shelley depth XPub -> ByteString
blake2b224 =
    BA.convert . hash @_ @Blake2b_224 . xpubPublicKey . getKey


-- Size, in bytes, of a hash of public key (without the corresponding chain code)
publicKeyHashSize :: Int
publicKeyHashSize = hashDigestSize Blake2b_224

-- Purpose is a constant set to 1852' (or 0x8000073c) following the BIP-44
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

-- One master node (seed) can be used for unlimited number of independent
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

-- The minimum seed length for 'genMasterKeyFromMnemonic'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16
