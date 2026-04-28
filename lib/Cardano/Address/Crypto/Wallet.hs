{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Vendored from @cardano-crypto@ (Apache-2.0).
-- Original: Cardano.Crypto.Wallet
--
-- HD Wallet routines using Ed25519 arithmetic.
module Cardano.Address.Crypto.Wallet
    ( ChainCode (..)
    , DerivationScheme (..)
    , DerivationIndex
    , pattern LatestScheme
    -- * Extended Private & Public types
    , XPrv
    , XPub (..)
    , XSignature
    , generate
    , generateNew
    , xprv
    , xpub
    , xsignature
    , unXPrv
    , unXPub
    , unXSignature
    , toXPub
    , xPubGetPublicKey
    , xPrvChangePass
    -- * Derivation function
    , deriveXPrv
    , deriveXPub
    -- * Signature & Verification from extended types
    , sign
    , verify
    ) where

import Prelude

import Control.Arrow
    ( second )
import Control.DeepSeq
    ( NFData )
import Crypto.Error
    ( CryptoError (..), CryptoFailable (..), throwCryptoError )
import Crypto.Hash
    ( SHA512 )
import Crypto.KDF.PBKDF2
    ( Parameters (..), fastPBKDF2_SHA512 )
import qualified Crypto.MAC.HMAC as HMAC
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ByteArray
    ( ByteArrayAccess, convert )
import qualified Data.ByteArray as BA
    ( append, length, splitAt )
import Data.ByteString
    ( ByteString )
import qualified Data.ByteString.Char8 as B8
import Data.Hashable
    ( Hashable )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

import Cardano.Address.Crypto.Wallet.Encrypted
    ( EncryptedKey
    , Signature (..)
    , encryptedChangePass
    , encryptedCreate
    , encryptedCreateDirectWithTweak
    , encryptedDerivePrivate
    , encryptedDerivePublic
    , encryptedKey
    , encryptedSign
    , unEncryptedKey
    )
import Cardano.Address.Crypto.Wallet.Types
    ( ChainCode (..)
    , DerivationIndex
    , DerivationScheme (..)
    , pattern LatestScheme
    )

newtype XPrv = XPrv EncryptedKey
    deriving newtype (NFData, ByteArrayAccess)

data XPub = XPub
    { xpubPublicKey :: !ByteString
    , xpubChaincode :: !ChainCode
    } deriving (Eq, Show, Ord, Generic)

instance NFData XPub
instance Hashable XPub

newtype XSignature = XSignature
    { unXSignature :: ByteString
    } deriving stock (Show, Eq, Ord)
      deriving newtype (NFData, Hashable, ByteArrayAccess)

-- | Generate a new XPrv (legacy Byron method).
--
-- The seed needs to be >= 32 bytes.
generate
    :: (ByteArrayAccess passPhrase, ByteArrayAccess seed)
    => seed -> passPhrase -> XPrv
generate seed passPhrase
    | BA.length seed < 32 =
        error ("Wallet.generate: seed needs to be >= 32 bytes, got : "
            ++ show (BA.length seed))
    | otherwise = loop 1
  where
    phrase :: Int -> ByteString
    phrase i = "Root Seed Chain " `BA.append` B8.pack (show i)

    loop i
        | i > 1000 =
            error "internal error: Wallet.generate looping forever"
        | otherwise =
            case encryptedCreate iL passPhrase iR of
                CryptoPassed k -> XPrv k
                CryptoFailed err
                    | err == CryptoError_SecretKeyStructureInvalid ->
                        loop (i + 1)
                    | otherwise ->
                        error "internal error: Wallet.generate: \
                              \got error from encryptedCreate"
      where
        (iL, iR) = hFinalize
            $ flip HMAC.update (phrase i)
            $ hInitSeed seed

-- | Generate a new XPrv from entropy (Icarus method).
--
-- The seed should be at least 16 bytes.
generateNew
    :: ( ByteArrayAccess keyPassPhrase
       , ByteArrayAccess generationPassPhrase
       , ByteArrayAccess seed
       )
    => seed
    -> generationPassPhrase
    -> keyPassPhrase
    -> XPrv
generateNew seed genPassPhrase memPassPhrase =
    XPrv $ encryptedCreateDirectWithTweak out memPassPhrase
  where
    out :: ByteString
    out = fastPBKDF2_SHA512 (Parameters 4096 96) genPassPhrase seed

xprv :: ByteArrayAccess bin => bin -> Either String XPrv
xprv bs =
    maybe
        (Left "error: xprv needs to be 128 bytes")
        (Right . XPrv)
        $ encryptedKey
        $ convert bs

xpub :: ByteString -> Either String XPub
xpub bs
    | BA.length bs /= 64 =
        Left ("error: xpub needs to be 64 bytes: got "
            ++ show (BA.length bs) ++ " bytes")
    | otherwise =
        let (b1, b2) = BA.splitAt 32 bs
         in Right $ XPub b1 (ChainCode $ convert b2)

unXPrv :: XPrv -> ByteString
unXPrv (XPrv e) = unEncryptedKey e

unXPub :: XPub -> ByteString
unXPub (XPub pub (ChainCode cc)) = BA.append pub cc

xsignature :: ByteString -> Either String XSignature
xsignature bs
    | BA.length bs /= 64 =
        Left ("error: xsignature needs to be 64 bytes: got "
            ++ show (BA.length bs) ++ " bytes")
    | otherwise = Right $ XSignature bs

toXPub :: HasCallStack => XPrv -> XPub
toXPub (XPrv ekey) = XPub pub (ChainCode cc)
  where
    (_, r) = BA.splitAt 64 $ convert ekey
    (pub, cc) = BA.splitAt 32 r

xPubGetPublicKey :: XPub -> Ed25519.PublicKey
xPubGetPublicKey (XPub pub _) =
    throwCryptoError $ Ed25519.publicKey pub

xPrvChangePass
    :: (ByteArrayAccess oldPassPhrase, ByteArrayAccess newPassPhrase)
    => oldPassPhrase -> newPassPhrase -> XPrv -> XPrv
xPrvChangePass oldPass newPass (XPrv ekey) =
    XPrv $ encryptedChangePass oldPass newPass ekey

deriveXPrv
    :: ByteArrayAccess passPhrase
    => DerivationScheme -> passPhrase -> XPrv -> Word32 -> XPrv
deriveXPrv ds passPhrase (XPrv ekey) n =
    XPrv (encryptedDerivePrivate ds ekey passPhrase n)

deriveXPub :: DerivationScheme -> XPub -> Word32 -> Maybe XPub
deriveXPub ds (XPub pub (ChainCode cc)) n
    | n >= 0x80000000 = Nothing
    | otherwise =
        Just $ uncurry XPub $ second ChainCode
            $ encryptedDerivePublic ds (pub, cc) n

sign
    :: (ByteArrayAccess passPhrase, ByteArrayAccess msg)
    => passPhrase -> XPrv -> msg -> XSignature
sign passphrase (XPrv ekey) ba =
    XSignature $ let (Signature sig) = encryptedSign ekey passphrase ba
                  in sig

verify :: ByteArrayAccess msg => XPub -> msg -> XSignature -> Bool
verify (XPub point _) ba (XSignature signature) =
    let pub = throwCryptoError $ Ed25519.publicKey point
        sig = throwCryptoError $ Ed25519.signature signature
     in Ed25519.verify pub ba sig

-- | HMAC-SHA512 helpers (inlined from Wallet.Pure to avoid
-- depending on Crypto.Math.Edwards25519)
hInitSeed :: ByteArrayAccess seed => seed -> HMAC.Context SHA512
hInitSeed = HMAC.initialize

hFinalize :: HMAC.Context SHA512 -> (ByteString, ChainCode)
hFinalize ctx =
    let (b1, b2) = BA.splitAt 32 $ convert $ HMAC.finalize ctx
     in (b1, ChainCode b2)
