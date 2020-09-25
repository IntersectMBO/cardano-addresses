{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Multisig
    (
      MultisigScript (..)
    , VerificationKeyHash (..)
    , fromVerificationKey
    , verificationKeyHashFromBytes
    , toScriptHash

    -- * Internal
    , toCBOR
    ) where

import Prelude

import Cardano.Address
    ( invariantSize )
import Cardano.Address.Derivation
    ( Depth (..), XPub )
import Cardano.Address.Style.Shelley
    ( Shelley, blake2b224, pubkeyHashSize )
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


-- | A 'MultisigScript' type represents multi signature script. The script embodies conditions
-- that need to be satisfied to make it valid.
--
-- @since 3.0.0
data MultisigScript =
      RequireSignatureOf VerificationKeyHash
    | RequireAllOf [MultisigScript]
    | RequireAnyOf [MultisigScript]
    | RequireMOf Word8 [MultisigScript]
    deriving stock (Generic, Show, Eq)

instance NFData MultisigScript

-- | A 'VerificationKeyHash' type represents verification key hash
-- that participate in building MultisigScript. The hash is expected to have size of
-- 28-byte.
--
-- @since 3.0.0
newtype VerificationKeyHash = VerificationKeyHash ByteString
    deriving (Generic, Show, Eq)

instance NFData VerificationKeyHash

fromVerificationKey :: Shelley 'MultisigK XPub -> VerificationKeyHash
fromVerificationKey = VerificationKeyHash . blake2b224

verificationKeyHashFromBytes :: ByteString -> VerificationKeyHash
verificationKeyHashFromBytes = VerificationKeyHash . invariantSize pubkeyHashSize

toCBOR' :: MultisigScript -> CBOR.Encoding
toCBOR' (RequireSignatureOf (VerificationKeyHash verKeyHash)) =
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
toCBOR :: MultisigScript -> ByteString
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
toScriptHash :: MultisigScript -> ByteString
toScriptHash script = digest $ nativeMultiSigTag <> toCBOR'' script
  where
      nativeMultiSigTag :: ByteString
      nativeMultiSigTag = "\00"

      digest = BA.convert . hash @_ @Blake2b_224

      toCBOR'' :: MultisigScript -> ByteString
      toCBOR'' script' = CBOR.toStrictByteString $ toCBOR' script'
