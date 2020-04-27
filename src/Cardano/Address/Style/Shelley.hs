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
    , publicKeyHashSize
    ) where

import Prelude

import Cardano.Address
    ( NetworkDiscriminant (..)
    , PaymentAddress (..)
    , ProtocolMagic (..)
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
    , xpubToBytes
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
    ( Blake2b_224 )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- $overview
--
-- This module provides an implementation of:
--
-- - 'GenMasterKey': for generating Shelley master keys from mnemonic sentences
-- - 'HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'SoftDerivation': for hierarchical soft derivation of parent to child keys
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
-- === Generating an 'Address' from a root key
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'/0/14@
--
--
-- > import Cardano.Address ( PaymentAddress(..), bech32, mainnetDiscriminant )
-- > import Cardano.Address.Derivation ( AccountingStyle(..), GenMasterKey(..), toXPub )
-- >
-- > let accIx = toEnum 0x80000000
-- > let acctK = deriveAccountPrivateKey rootK accIx
-- >
-- > let addIx = toEnum 0x00000014
-- > let addrK = deriveAddressPrivateKey acctK UTxOExternal addIx
-- >
-- > bech32 $ paymentAddress mainnetDiscriminant (toXPub <$> addrK)
-- > "addr1vzu630t7mpaaqf7l8a0ave287f3wdvvduumt65jf4vnua2qcuevus"

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

instance PaymentAddress Shelley where
    paymentAddress discrimination k = unsafeMkAddress $
        invariantSize $ BL.toStrict $ runPut $ do
            putWord8 firstByte
            putByteString hashedKey
      where
          -- we use here the fact that payment address stands for what is named
          -- as enterprise address, ie., address carrying no stake rights. For
          -- rationale why we may need such addresses refer to delegation
          -- specification - Section 3.2.3. What is important here is that the
          -- address is composed of discrimination byte and 28 bytes hashed public key.
          -- Moreover, it was decided that first 4 bits for enterprise address
          -- will be `0110`. The next for bits for network discriminator are to
          -- be established soon. TO_DO : change `firstbyte` as network
          -- discriminant in CDDL spec is known.
          firstByte = case discrimination of
              RequiresNoMagic  -> 0x60
              RequiresMagic (ProtocolMagic _pm) -> 0x61
          hashedKey = BA.convert $ hash @_ @Blake2b_224 $ xpubToBytes $ getKey k
          expectedLength = 1 + publicKeyHashSize
          invariantSize :: HasCallStack => ByteString -> ByteString
          invariantSize bytes
              | BS.length bytes == expectedLength = bytes
              | otherwise = error
                $ "length was "
                ++ show (BS.length bytes)
                ++ ", but expected to be "
                ++ (show expectedLength)

{-------------------------------------------------------------------------------
                                 Key generation
-------------------------------------------------------------------------------}

-- | Size, in bytes, of a hash of public key (without chain code)
publicKeyHashSize :: Int
publicKeyHashSize = 28

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
