{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module System.IO.Extra
    (
    -- * I/O application-specific helpers
    -- ** Read
      hGetBytes
    , hGetSomeMnemonic
    , hGetXPrv
    , hGetXPub
    , hGetXP__

    -- ** Write
    , hPutBold
    , hPutBytes

    -- * I/O Helpers
    , prettyIOException
    , withSGR
    , markCharsRedAtIndices
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xprvFromBytes, xpubFromBytes )
import Cardano.Mnemonic
    ( MkSomeMnemonicError (..), SomeMnemonic, mkSomeMnemonic )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( left )
import Control.Exception
    ( IOException, bracket )
import Control.Monad
    ( guard )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58, unAlphabet )
import Data.Char
    ( isDigit, isLower, toLower )
import Data.List
    ( nub, sort )
import Options.Applicative.Encoding
    ( AbstractEncoding (..), Encoding )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , hSetSGR
    , setSGRCode
    )
import System.Exit
    ( exitFailure )
import System.IO
    ( Handle, hIsTerminalDevice, stderr )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


--
-- I/O Read
--

-- | Read some bytes from the console, and decode them if the encoding is recognized.
hGetBytes :: Handle -> IO ByteString
hGetBytes h = do
    raw <- B8.filter (/= '\n') <$> B8.hGetContents h
    case detectEncoding (string raw) of
        Just (EBase16  ) -> hGetBase16 raw
        Just (EBech32{}) -> hGetBech32 raw
        Just (EBase58  ) -> hGetBase58 raw
        Nothing          -> fail
            "Couldn't detect input encoding? Data on stdin must be encoded as \
            \bech16, bech32 or base58."
  where
    detectEncoding :: String -> Maybe (AbstractEncoding ())
    detectEncoding str = isBase16 <|> isBech32  <|> isBase58
      where
        isBase16 = do
            guard (all (`elem` "0123456789abcdef") (toLower <$> str))
            guard (length str `mod` 2 == 0)
            pure EBase16

        isBech32 = do
            guard (all (`elem` Bech32.dataCharList) (stripPrefix str))
            guard (all (\c -> isLower c || isDigit c) str)
            guard ('1' `elem` str)
            guard (length (stripPrefix str) > 6)
            pure (EBech32 ())
          where
            stripPrefix = reverse . takeWhile (/= '1') . reverse

        isBase58 = do
            guard (all (`elem` string (unAlphabet bitcoinAlphabet)) str)
            pure EBase58

    string :: ByteString -> String
    string = T.unpack . T.decodeUtf8

    hGetBech32 :: ByteString -> IO ByteString
    hGetBech32 raw = either (fail . errToString) pure $ do
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
                invalidCharsMsg <> ":\n" <> markCharsRedAtIndices (map unCharPos ixs) (string raw)
            Nothing ->
                "invalid data-part; these bytes ain't uint8."

    hGetBase58 :: ByteString -> IO ByteString
    hGetBase58 raw = maybe (fail "Invalid Base58-encoded string.") pure $ do
        decodeBase58 bitcoinAlphabet raw

    hGetBase16 :: ByteString -> IO ByteString
    hGetBase16 raw = either fail pure $ do
        convertFromBase Base16 raw

-- | Read some English mnemonic words from the console, or fail.
hGetSomeMnemonic :: Handle -> IO SomeMnemonic
hGetSomeMnemonic h = do
    wrds <- T.words . trim '\n' . T.decodeUtf8 <$> B8.hGetContents h
    case mkSomeMnemonic @'[ 9, 12, 15, 18, 21, 24 ] wrds of
        Left (MkSomeMnemonicError e) -> fail e
        Right mw -> pure mw
  where
    trim c = T.filter (/= c)

-- | Read an encoded private key from the console, or fail.
hGetXPrv :: Handle -> IO XPrv
hGetXPrv h = do
    bytes <- hGetBytes h
    case xprvFromBytes bytes of
        Nothing  -> fail "Couldn't convert bytes into extended private key."
        Just key -> pure key

-- | Read an encoded public key from the console, or fail.
hGetXPub :: Handle -> IO XPub
hGetXPub h = do
    bytes <- hGetBytes h
    case xpubFromBytes bytes of
        Nothing  -> fail "Couldn't convert bytes into extended public key."
        Just key -> pure key

-- | Read either an encoded public or private key from the console, or fail.
hGetXP__ :: Handle -> IO (Either XPub XPrv)
hGetXP__ h = do
    bytes <- hGetBytes h
    case (xpubFromBytes bytes, xprvFromBytes bytes) of
        (Just xpub,         _) -> pure (Left  xpub)
        (_        , Just xprv) -> pure (Right xprv)
        _                      -> fail
            "Couldn't convert bytes into neither extended public or private keys."


--
-- I/O Write
--

-- | Print bytes to the console with the given encoding.
hPutBytes :: Handle -> ByteString -> Encoding -> IO ()
hPutBytes h bytes = \case
    EBase16 ->
        B8.hPutStr h $ convertToBase Base16 bytes
    EBase58 ->
        B8.hPutStr h $ encodeBase58 bitcoinAlphabet bytes
    EBech32 hrp ->
        B8.hPutStr h $ T.encodeUtf8 $ Bech32.encodeLenient hrp $ Bech32.dataPartFromBytes bytes

-- | Print bytes to the console, but in bold.
hPutBold :: Handle -> ByteString -> IO ()
hPutBold h bytes =
    withSGR h (SetConsoleIntensity BoldIntensity) $ B8.hPutStr h bytes


--
-- Helpers
--

-- | Fail with a colored red error message.
prettyIOException :: IOException -> IO a
prettyIOException e = do
    B8.hPutStrLn stderr $ T.encodeUtf8 $ T.pack $ show e
    exitFailure

-- | Bracket-style constructor for applying ANSI Select Graphic Rendition to an
-- action and revert back to normal after.
--
-- This does nothing if the device isn't an ANSI terminal.
withSGR :: Handle -> SGR -> IO a -> IO a
withSGR h sgr action = hIsTerminalDevice h >>= \case
    True  -> bracket aFirst aLast aBetween
    False -> action
  where
    aFirst = ([] <$ hSetSGR h [sgr])
    aLast = hSetSGR h
    aBetween = const action

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
