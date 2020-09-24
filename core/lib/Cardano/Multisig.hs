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
    , unsafeVerificationKeyHash
    , toCBOR
    , toScriptHash
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

newtype VerificationKeyHash = VerificationKeyHash ByteString
    deriving (Generic, Show, Eq)

instance NFData VerificationKeyHash

fromVerificationKey :: Shelley 'MultisigK XPub -> VerificationKeyHash
fromVerificationKey = VerificationKeyHash . blake2b224

unsafeVerificationKeyHash :: ByteString -> VerificationKeyHash
unsafeVerificationKeyHash = VerificationKeyHash . invariantSize pubkeyHashSize

toCBOR :: MultisigScript -> ByteString
toCBOR (RequireSignatureOf (VerificationKeyHash verKeyHash)) =
    let encoding = CBOR.encodeListLen 2
            <> CBOR.encodeWord8 0
            <> CBOR.encodeListLen 2
            <> CBOR.encodeWord 0
            <> CBOR.encodeBytes verKeyHash
    in CBOR.toStrictByteString encoding
toCBOR _ = undefined

encodeFoldable :: (Foldable f) => (a -> CBOR.Encoding) -> f a -> CBOR.Encoding
encodeFoldable encode xs = wrapArray len contents
  where
    (len, contents) = foldl' go (0, mempty) xs
    go (!l, !enc) next = (l + 1, enc <> encode next)

    wrapArray :: Word -> CBOR.Encoding -> CBOR.Encoding
    wrapArray l content =
        if l <= 23
        then CBOR.encodeListLen l <> content
        else CBOR.encodeListLenIndef <> content <> CBOR.encodeBreak

toScriptHash :: MultisigScript -> ByteString
toScriptHash script = digest $ nativeMultiSigTag <> toCBOR script
  where
      nativeMultiSigTag :: ByteString
      nativeMultiSigTag = "\00"

      digest = BA.convert . hash @_ @Blake2b_224
