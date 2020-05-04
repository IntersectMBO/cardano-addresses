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

      -- * Discrimination
    , MkNetworkDiscriminantError (..)
    , mkNetworkDiscriminant

      -- * Accessors
    , getKey

      -- * Unsafe
    , liftXPrv

      -- Internals
    , minSeedLengthBytes
    , publicKeyHashSize
    ) where

import Prelude

import Cardano.Address
    ( AddressDiscrimination (..)
    , DelegationAddress (..)
    , NetworkDiscriminant (..)
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
    , XPub
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
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BL

-- $overview
--
-- This module provides an implementation of:
--
-- - 'GenMasterKey': for generating Shelley master keys from mnemonic sentences
-- - 'HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'SoftDerivation': for hierarchical soft derivation of parent to child keys
-- - 'PaymentAddress': for constructing payment addresses from a address public key
-- - 'DelegationAddress': for constructing delegation addresses from address and staking public keys
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
-- > let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
--
-- === Generating an 'PaymentAddress' and 'DelegationAddress' from a root key
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'/0/14@
--
--
-- > import Cardano.Address ( PaymentAddress(..), DelegationAddress (..), bech32)
-- > import Cardano.Address.Derivation ( AccountingStyle(..), GenMasterKey(..), toXPub )
-- >
-- > let accIx = toEnum 0x80000000
-- > let acctK = deriveAccountPrivateKey rootK accIx
-- >
-- > let addIx = toEnum 0x00000014
-- > let addrK = deriveAddressPrivateKey acctK UTxOExternal addIx
-- >
-- > let (Right netTag) = mkNetworkDiscriminant 1
-- > bech32 $ paymentAddress netTag (toXPub <$> addrK)
-- > "addr1vxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdncxsce5t"
-- >
-- > let stakeK = deriveStakingPrivateKey acctK
-- > bech32 $ delegationAddress netTag (toXPub <$> addrK) (toXPub <$> stakeK)
-- > "addr1qxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdn7nudck0fzve4346yytz3wpwv9yhlxt7jwuc7ytwx2vfkyqmkc5xa"

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

instance GenMasterKey Shelley where
    type SecondFactor Shelley = ScrubbedBytes

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic fstFactor sndFactor =
        Shelley $ generateNew seedValidated sndFactor
        where
            seed  = someMnemonicToBytes fstFactor
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

instance PaymentAddress Shelley where
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
              96 + invariantNetworkTag (networkTag @Shelley discrimination)
          expectedLength = 1 + publicKeyHashSize

instance DelegationAddress Shelley where
    delegationAddress discrimination paymentKey stakingKey = unsafeMkAddress $
        invariantSize expectedLength $ BL.toStrict $ runPut $ do
            putWord8 firstByte
            putByteString . blake2b224 $ paymentKey
            putByteString . blake2b224 $ stakingKey
      where
          -- we use here the fact that payment address stands for what is named
          -- as base address - refer to delegation specification - Section 3.2.1.
          -- What is important here is that the address is composed of discrimination
          -- byte and two 28 bytes hashed public keys, one for payment key and the
          -- other for staking/reword key.
          -- Moreover, it was decided that first 4 bits for enterprise address
          -- will be `0000`. The next for bits are reserved for network discriminator.
          firstByte =
              invariantNetworkTag (networkTag @Shelley discrimination)
          expectedLength = 1 + 2*publicKeyHashSize

    deriveStakingPrivateKey (Shelley accXPrv) =
        let
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv (toEnum @(Index 'Soft _) 2)
            stakeXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv (minBound @(Index 'Soft _))
        in
            Shelley stakeXPrv

invariantNetworkTag :: HasCallStack => NetworkTag -> Word8
invariantNetworkTag (NetworkTag num)
    | num < 16 = fromIntegral num
    | otherwise = error
      $ "network tag was "
      ++ show num
      ++ ", but expected to be less than 16"

blake2b224 :: Shelley depth XPub -> ByteString
blake2b224 =
    BA.convert . hash @_ @Blake2b_224 . getPublicKey . getKey

{-------------------------------------------------------------------------------
                                 Key generation
-------------------------------------------------------------------------------}

-- | Size, in bytes, of a hash of public key (without the corresponding chain code)
publicKeyHashSize :: Int
publicKeyHashSize = hashDigestSize Blake2b_224

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
