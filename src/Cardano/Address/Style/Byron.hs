{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0

module Cardano.Address.Style.Byron
    ( -- $overview
      -- * Byron
      Byron
    , DerivationPath

      -- * Accessors
    , payloadPassphrase
    , derivationPath
    , getKey

      -- * Unsafe
    , liftXPrv

      -- Internal
    , minSeedLengthBytes
    ) where

import Prelude

import Cardano.Address
    ( HasNetworkDiscriminant (..)
    , NetworkDiscriminantByron (..)
    , PaymentAddress (..)
    , ProtocolMagic (..)
    , unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationScheme (DerivationScheme1)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index
    , XPrv
    , XPub
    , deriveXPrv
    , generate
    , toXPub
    , xpubToBytes
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy )
import Control.DeepSeq
    ( NFData )
import Control.Exception.Base
    ( assert )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256, SHA512 (..) )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Codec.Cbor as CBOR
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.ByteArray as BA

-- $overview
--
-- This module provides an implementation of:
--
-- - 'GenMasterKey': for generating Byron master keys from mnemonic sentences
-- - 'HardDerivation': for hierarchical derivation of parent to child keys
-- - 'PaymentAddress': for constructing addresses from a public key
--
-- We call 'Byron' addresses the old address type used by Daedalus in the early
-- days of Cardano. Using this type of addresses and underlying key scheme is
-- now considered __deprecated__ because of some security implications.
--
-- Unless you have good reason to do so (like writing backward-compatible code
-- with an existing piece), any new implementation __should use__ the 'Icarus'
-- style for key and addresses.
--
-- The internals of the 'Byron' does not matter for the reader, but basically
-- contains what is necessary to perform key derivation and generate addresses
-- from a 'Byron' type.

{-------------------------------------------------------------------------------
                                   Key Types
-------------------------------------------------------------------------------}

-- | Material for deriving HD random scheme keys, which can be used for making
-- addresses.
--
-- @since 1.0.0
data Byron (depth :: Depth) key = Byron
    { getKey :: key
    -- ^ The raw private or public key.
    --
    -- @since 1.0.0
    , derivationPath :: DerivationPath depth
    -- ^ The address derivation indices for the level of this key.
    --
    -- @since 1.0.0
    , payloadPassphrase :: ScrubbedBytes
    -- ^ Used for encryption of the derivation path payload within an address.
    --
    -- @since 1.0.0
    } deriving stock (Generic)
{-# DEPRECATED Byron "see Cardano.Address.Style.Icarus" #-}
{-# DEPRECATED getKey "see Cardano.Address.Style.Icarus" #-}
{-# DEPRECATED derivationPath "see Cardano.Address.Style.Icarus" #-}
{-# DEPRECATED payloadPassphrase "see Cardano.Address.Style.Icarus" #-}

instance (NFData key, NFData (DerivationPath depth)) => NFData (Byron depth key)
deriving instance (Show key, Show (DerivationPath depth)) => Show (Byron depth key)
deriving instance (Eq key, Eq (DerivationPath depth)) => Eq (Byron depth key)
deriving instance (Functor (Byron depth))

-- | Backdoor for generating a new key from a raw XPrv.
--
-- Note that the @depth@ is left open so that the caller gets to decide what type
-- of key this is. This is mostly for testing, in practice, seeds are used to
-- represent root keys, and one should 'genMasterKeyFromXPrv'
--
-- The first argument is a type-family 'DerivationPath' and its type depends on
-- the 'depth' of the key.
--
-- __examples:__
--
-- >>> liftXPrv () prv
-- _ :: Byron RootK XPrv
--
-- >>> liftXPrv minBound prv
-- _ :: Byron AccountK XPrv
--
-- >>> liftXPrv (minBound, minBound) prv
-- _ :: Byron AddressK XPrv
--
-- @since 1.0.0
liftXPrv
    :: DerivationPath depth
    -> XPrv
    -> Byron depth XPrv
liftXPrv derivationPath getKey = Byron
    { getKey
    , derivationPath
    , payloadPassphrase = hdPassphrase (toXPub getKey)
    }
{-# DEPRECATED liftXPrv "see Cardano.Address.Style.Icarus" #-}

-- | The hierarchical derivation indices for a given level/depth.
--
-- @since 1.0.0
type family DerivationPath (depth :: Depth) :: * where
    -- The root key is generated from the seed.
    DerivationPath 'RootK =
        ()
    -- The account key is generated from the root key and account index.
    DerivationPath 'AccountK =
        Index 'WholeDomain 'AccountK
    -- The address key is generated from the account key and address index.
    DerivationPath 'AddressK =
        (Index 'WholeDomain 'AccountK, Index 'WholeDomain 'AddressK)
{-# DEPRECATED DerivationPath "see Cardano.Address.Style.Icarus" #-}

instance GenMasterKey Byron where
    type SecondFactor Byron = ()

    genMasterKeyFromXPrv = liftXPrv ()
    genMasterKeyFromMnemonic (SomeMnemonic mw) () =
        liftXPrv () xprv
      where
        xprv = generate (hashSeed seedValidated)
        seed  = entropyToBytes $ mnemonicToEntropy mw
        seedValidated = assert
            (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
            seed

instance HardDerivation Byron where
    type AddressIndexDerivationType Byron = 'WholeDomain
    type AccountIndexDerivationType Byron = 'WholeDomain
    type WithAccountStyle Byron = ()

    deriveAccountPrivateKey rootXPrv accIx = Byron
        { getKey = deriveXPrv DerivationScheme1 (getKey rootXPrv) accIx
        , derivationPath = accIx
        , payloadPassphrase = payloadPassphrase rootXPrv
        }

    deriveAddressPrivateKey accXPrv () addrIx = Byron
        { getKey = deriveXPrv DerivationScheme1 (getKey accXPrv) addrIx
        , derivationPath = (derivationPath accXPrv, addrIx)
        , payloadPassphrase = payloadPassphrase accXPrv
        }

instance HasNetworkDiscriminant Byron where
    type NetworkDiscriminant Byron = NetworkDiscriminantByron
    type NetworkNumber Byron = Word32

    requiredInAddress RequiresNoMagic = False
    requiredInAddress (RequiresMagic _) = True

    networkNumber (RequiresMagic (ProtocolMagic pm)) = pm
    networkNumber RequiresNoMagic = error "no asking for magic tag here"

instance PaymentAddress Byron where
    paymentAddress discrimination k = unsafeMkAddress
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getKey k) attrs
      where
        (acctIx, addrIx) = bimap word32 word32 $ derivationPath k
        pwd = payloadPassphrase k
        attrs = if requiredInAddress @Byron discrimination then
            [ CBOR.encodeDerivationPathAttr pwd acctIx addrIx
            , CBOR.encodeProtocolMagicAttr (networkNumber @Byron discrimination)
            ]
            else
            [ CBOR.encodeDerivationPathAttr pwd acctIx addrIx
            ]

--
-- Internal
--

-- The amount of entropy carried by a BIP-39 12-word mnemonic is 16 bytes.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- | Hash the seed entropy (generated from mnemonic) used to initiate a HD
-- wallet. This increases the key length to 34 bytes, selectKey is greater than the
-- minimum for 'generate' (32 bytes).
--
-- Note that our current implementation deviates from BIP-39 because we use a
-- hash function (Blake2b) rather than key stretching with PBKDF2.
--
-- There are two methods of hashing the seed entropy, for different use cases.
--
-- 1. Normal random derivation wallet seeds. The seed entropy is hashed using
--    Blake2b_256, inside a double CBOR serialization sandwich.
--
-- 2. Seeds for redeeming paper wallets. The seed entropy is hashed using
--    Blake2b_256, without any serialization.
hashSeed :: ScrubbedBytes -> ScrubbedBytes
hashSeed = serialize . blake2b256 . serialize
  where
    serialize = BA.convert . cbor . BA.convert
    cbor = CBOR.toStrictByteString . CBOR.encodeBytes

blake2b256 :: ScrubbedBytes -> ScrubbedBytes
blake2b256 = BA.convert . hash @ScrubbedBytes @Blake2b_256

-- | Derive a symmetric key for encrypting and authenticating the address
-- derivation path. PBKDF2 encryption using HMAC with the hash algorithm SHA512
-- is employed.
hdPassphrase :: XPub -> ScrubbedBytes
hdPassphrase masterKey =
    PBKDF2.generate
    (PBKDF2.prfHMAC SHA512)
    (PBKDF2.Parameters 500 32)
    (xpubToBytes masterKey)
    ("address-hashing" :: ByteString)

word32 :: Enum a => a -> Word32
word32 = fromIntegral . fromEnum
