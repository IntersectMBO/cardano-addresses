{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Multisig
    (
      Script (..)
    , KeyHash (..)
    , hashKey
    , keyHashFromBytes
    , toScriptHash

    -- * Internal
    , toCBOR
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub )
import Cardano.Address.Style.Shelley
    ( Shelley, blake2b224, hashSize )
import Control.DeepSeq
    ( NFData )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_224 )
import Data.ByteString
    ( ByteString )
import Data.Foldable
    ( foldl' )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Codec.Cbor as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS


-- | A 'Script' type represents multi signature script. The script embodies conditions
-- that need to be satisfied to make it valid.
--
-- @since 3.0.0
data Script =
      RequireSignatureOf KeyHash
    | RequireAllOf [Script]
    | RequireAnyOf [Script]
    | RequireMOf Word8 [Script]
    deriving stock (Generic, Show, Eq)

instance NFData Script

-- | A 'KeyHash' type represents verification key hash
-- that participate in building MultisigScript. The hash is expected to have size of
-- 28-byte.
--
-- @since 3.0.0
newtype KeyHash = KeyHash ByteString
    deriving (Generic, Show, Eq)

instance NFData KeyHash

-- | Apply Blake2b224 hashing on Shelley 'XPub'.
-- This results in 28-byte hash.
--
-- @since 3.0.0
hashKey :: Shelley key XPub -> KeyHash
hashKey = KeyHash . blake2b224

-- | Construct an 'KeyHash' from raw 'ByteString' (28 bytes).
--
-- @since 3.0.0
keyHashFromBytes :: ByteString -> Maybe KeyHash
keyHashFromBytes bytes
    | BS.length bytes /= hashSize = Nothing
    | otherwise = Just $ KeyHash bytes

toCBOR' :: Script -> CBOR.Encoding
toCBOR' (RequireSignatureOf (KeyHash verKeyHash)) =
    encodeMultiscriptCtr 0 2 <> CBOR.encodeBytes verKeyHash
toCBOR' (RequireAllOf contents) =
    encodeMultiscriptCtr 1 2 <> encodeFoldable toCBOR' contents
toCBOR' (RequireAnyOf contents) =
    encodeMultiscriptCtr 2 2 <> encodeFoldable toCBOR' contents
toCBOR' (RequireMOf m contents) =
    encodeMultiscriptCtr 3 3 <> CBOR.encodeInt (fromInteger $ toInteger m)
    <> encodeFoldable toCBOR' contents

-- | This function realizes what cardano-node's `Api.serialiseToCBOR script` realizes
-- This is basically doing the symbolically following:
-- toCBOR [0,multisigScript]
toCBOR :: Script -> ByteString
toCBOR script =
    CBOR.toStrictByteString $ encodeMultisigBeginning <> toCBOR' script

encodeMultisigBeginning :: CBOR.Encoding
encodeMultisigBeginning = CBOR.encodeListLen 2 <> CBOR.encodeWord8 0

encodeMultiscriptCtr :: Word -> Word -> CBOR.Encoding
encodeMultiscriptCtr ctrIndex listLen =
    CBOR.encodeListLen listLen <> CBOR.encodeWord ctrIndex

encodeFoldable :: (Foldable f) => (a -> CBOR.Encoding) -> f a -> CBOR.Encoding
encodeFoldable encode xs = wrapArray len contents
  where
    (len, contents) = foldl' go (0, mempty) xs
    go (!l, !enc) next = (l + 1, enc <> encode next)

    wrapArray :: Word -> CBOR.Encoding -> CBOR.Encoding
    wrapArray len' contents' =
        if len' <= 23
        then CBOR.encodeListLen len' <> contents'
        else CBOR.encodeListLenIndef <> contents' <> CBOR.encodeBreak

-- | This function realizes what cardano-node's
-- `Api.serialiseToRawBytes $ Api.scriptHash script` realizes
-- This is basically doing the symbolically the following (using symbols from toCBOR):
-- digest $ (nativeMultisigTag <> toCBOR multisigScript )
toScriptHash :: Script -> ByteString
toScriptHash script = digest $ nativeMultiSigTag <> toCBOR'' script
  where
      nativeMultiSigTag :: ByteString
      nativeMultiSigTag = "\00"

      digest = BA.convert . hash @_ @Blake2b_224

      toCBOR'' :: Script -> ByteString
      toCBOR'' script' = CBOR.toStrictByteString $ toCBOR' script'
