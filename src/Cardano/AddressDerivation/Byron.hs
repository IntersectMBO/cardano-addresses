{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for the random scheme, as
-- implemented by the legacy Cardano wallets.
--
-- For full documentation of the key derivation schemes,
-- see the "Cardano.Crypto.Wallet" module, and the implementation in
-- <https://github.com/input-output-hk/cardano-crypto/blob/4590efa638397e952a51a8994b5543e4ea3c1ecd/cbits/encrypted_sign.c cardano-crypto>.

module Cardano.AddressDerivation.Byron
    ( -- * Types
      Byron(..)

      -- * Generation
    , unsafeGenerateKeyFromSeed
    , minSeedLengthBytes
    , mkByronKeyFromMasterKey

    ) where

import Prelude

import Cardano.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index (..)
    )
import Cardano.Crypto.Wallet
    ( DerivationScheme (DerivationScheme1)
    , XPrv
    , XPub
    , deriveXPrv
    , generate
    , toXPub
    , unXPub
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
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import GHC.Generics
    ( Generic )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.ByteArray as BA

{-------------------------------------------------------------------------------
                                   Key Types
-------------------------------------------------------------------------------}

-- | Material for deriving HD random scheme keys, which can be used for making
-- addresses.
data Byron (depth :: Depth) key = Byron
    { getKey :: key
    -- ^ The raw private or public key.
    , payloadPassphrase :: ScrubbedBytes
    -- ^ Used for encryption of payload containing address derivation path.
    } deriving stock (Generic)

instance (NFData key) => NFData (Byron depth key)
deriving instance (Show key) => Show (Byron depth key)
deriving instance (Eq key) => Eq (Byron depth key)

instance GenMasterKey Byron where
    type GenMasterKeyFrom Byron = SomeMnemonic
    genMasterKey = unsafeGenerateKeyFromSeed

instance HardDerivation Byron where
    type AddressIndexDerivationType Byron = 'WholeDomain
    type AccountIndexDerivationType Byron = 'WholeDomain
    deriveAccountPrivateKey = deriveAccountPrivateKeyImpl
    deriveAddressPrivateKey pwd accXPrv _accStyle =
        deriveAddressPrivateKeyImpl pwd accXPrv

{-------------------------------------------------------------------------------
                                 Key generation
-------------------------------------------------------------------------------}

-- | The amount of entropy carried by a BIP-39 12-word mnemonic is 16 bytes.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
unsafeGenerateKeyFromSeed
    :: SomeMnemonic
    -> ScrubbedBytes
    -> Byron 'RootK XPrv
unsafeGenerateKeyFromSeed (SomeMnemonic mw) pwd = Byron
    { getKey = masterKey
    , payloadPassphrase = hdPassphrase (toXPub masterKey)
    }
  where
    masterKey = generate (hashSeed seedValidated) pwd
    seed  = entropyToBytes $ mnemonicToEntropy mw
    seedValidated = assert
        (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
        seed


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
    (unXPub masterKey)
    ("address-hashing" :: ByteString)

mkByronKeyFromMasterKey
    :: XPrv
    -> Byron 'RootK XPrv
mkByronKeyFromMasterKey masterKey = Byron
    { getKey = masterKey
    , payloadPassphrase = hdPassphrase (toXPub masterKey)
    }

{-------------------------------------------------------------------------------
                                 HD derivation
-------------------------------------------------------------------------------}

-- | Derives account private key from the given root private key, using
-- derivation scheme 1.
deriveAccountPrivateKeyImpl
    :: ScrubbedBytes
    -> Byron 'RootK XPrv
    -> Index 'WholeDomain 'AccountK
    -> Byron 'AccountK XPrv
deriveAccountPrivateKeyImpl pwd masterKey (Index accIx) = Byron
    { getKey = deriveXPrv DerivationScheme1 pwd (getKey masterKey) accIx
    , payloadPassphrase = payloadPassphrase masterKey
    }

-- | Derives address private key from the given account private key, using
-- derivation scheme 1.
deriveAddressPrivateKeyImpl
    :: ScrubbedBytes
    -> Byron 'AccountK XPrv
    -> Index 'WholeDomain 'AddressK
    -> Byron 'AddressK XPrv
deriveAddressPrivateKeyImpl pwd accountKey (Index addrIx) = Byron
    { getKey = deriveXPrv DerivationScheme1 pwd (getKey accountKey) addrIx
    , payloadPassphrase = payloadPassphrase accountKey
    }
