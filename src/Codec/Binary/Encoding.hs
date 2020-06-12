{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

module Codec.Binary.Encoding
    ( -- * Types
      AbstractEncoding (..)
    , Encoding

      -- * Encode
    , encode

      -- * Decode
    , detectEncoding
    , fromBase16
    , fromBase58
    , fromBech32

      -- * Internal
    , markCharsRedAtIndices
    ) where

import Prelude

import Codec.Binary.Bech32
    ( HumanReadablePart )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( left )
import Control.Monad
    ( guard )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58, unAlphabet )
import Data.Char
    ( isLetter, isLower, isUpper, ord, toLower )
import Data.List
    ( nub, sort )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , setSGRCode
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


--
-- Encoding
--

-- | A concrete 'Encoding' algebraic data-type.
type Encoding = AbstractEncoding HumanReadablePart

-- | An abstract 'Encoding' to make it easy to map over the bech32 component.
-- Typically used as 'AbstractEncoding HumanReadablePart'.
--
-- > λ> let xpubHRP = [humanReadablePart|xpub|]
-- > λ> let xprvHRP = [humanReadablePart|xprv|]
-- >
-- > λ> fmap (const xpubHRP) (EBech32 xprvHRP)
-- > EBech32 (HumanReadablePart "xpub")
--
data AbstractEncoding a
    = EBase16
    | EBase58
    | EBech32 a
    deriving (Eq, Show, Functor)

--
-- Encode
--


-- | Encode a 'ByteString' with the given encoding.
--
-- @since 2.0.0
encode :: Encoding -> ByteString -> ByteString
encode encoding bytes = case encoding of
    EBase16 ->
        convertToBase Base16 bytes
    EBase58 ->
        encodeBase58 bitcoinAlphabet bytes
    EBech32 hrp ->
        T.encodeUtf8 $ Bech32.encodeLenient hrp $ Bech32.dataPartFromBytes bytes

--
-- Decode
--

-- | Try detecting the encoding of a given 'String'
--
-- @since 2.0.0
detectEncoding :: String -> Maybe (AbstractEncoding ())
detectEncoding str = isBase16 <|> isBech32  <|> isBase58
  where
    isBase16 = do
        guard (all (`elem` "0123456789abcdef") (toLower <$> str))
        guard (even (length str))
        pure EBase16

    isBech32 = do
        guard (not (null humanpart))
        guard (all (\c -> ord c >= 33 && ord c <= 126) humanpart)
        guard (length datapart >= 6)
        guard (all (`elem` Bech32.dataCharList) datapart)
        guard (all isUpper alpha || all isLower alpha)
        pure (EBech32 ())
      where
        datapart  = reverse . takeWhile (/= '1') . reverse $ str
        humanpart = takeWhile (/= '1') str
        alpha = filter isLetter str

    isBase58 = do
        guard (all (`elem` T.unpack (T.decodeUtf8 $ unAlphabet bitcoinAlphabet)) str)
        pure EBase58

-- | Try decoding a base16-encoded 'ByteString'
--
-- @since 2.0.0
fromBase16 :: ByteString -> Either String ByteString
fromBase16 = convertFromBase Base16

-- | Try decoding a bech32-encoded 'ByteString'
--
-- @since 2.0.0
fromBech32 :: ByteString -> Either String ByteString
fromBech32 raw = left errToString $ do
    (_hrp, dp) <- left Just $ Bech32.decodeLenient $ T.decodeUtf8 raw
    maybe (Left Nothing) Right $ Bech32.dataPartToBytes dp
  where
    unCharPos (Bech32.CharPosition x) = x
    invalidCharsMsg = "Invalid character(s) in string"
    errToString = ("Bech32 error: " <>) . \case
        Just Bech32.StringToDecodeTooLong ->
            "string is too long"
        Just Bech32.StringToDecodeTooShort ->
            "string is too short"
        Just Bech32.StringToDecodeHasMixedCase ->
            "string has mixed case"
        Just Bech32.StringToDecodeMissingSeparatorChar ->
            "string has no separator char"
        Just (Bech32.StringToDecodeContainsInvalidChars []) ->
            invalidCharsMsg
        Just (Bech32.StringToDecodeContainsInvalidChars ixs) ->
            invalidCharsMsg <> ":\n" <> markCharsRedAtIndices
                (map unCharPos ixs)
                (T.unpack . T.decodeUtf8 $ raw)
        Nothing ->
            "invalid data-part; these bytes ain't uint8."

fromBase58 :: ByteString -> Either String ByteString
fromBase58 raw = maybe (Left "Invalid Base58-encoded string.") Right $ do
    decodeBase58 bitcoinAlphabet raw

--
-- Helpers
--

-- | Mark all characters from a given string as red (in a console).
markCharsRedAtIndices :: Integral i => [i] -> String -> String
markCharsRedAtIndices ixs = go 0 (sort $ nub ixs)
  where
    go _c [] [] = mempty
    go c (i:is) (s:ss)
        | c == i    = red ++ s:def ++ go (c + 1) is ss
        | otherwise = s : go (c + 1) (i:is) ss
    go _ [] ss = ss
    go _ _ [] = [] -- NOTE: Really an error case.

    red = setSGRCode [SetColor Foreground Vivid Red]
    def = setSGRCode [Reset]
