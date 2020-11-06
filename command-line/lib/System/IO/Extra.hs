{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module System.IO.Extra
    (
    -- * I/O application-specific helpers
    -- ** Read
      hGetBytes
    , hGetBech32
    , hGetSomeMnemonic
    , hGetXPrv
    , hGetXPub
    , hGetXP__
    , hGetScriptHash

    -- ** Write
    , hPutBytes

    -- * I/O Helpers
    , prettyIOException
    , progName
    , markCharsRedAtIndices
    , noNewline
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xprvFromBytes, xpubFromBytes )
import Cardano.Address.Script
    ( ScriptHash, scriptHashFromBytes )
import Cardano.Mnemonic
    ( MkSomeMnemonicError (..), SomeMnemonic, mkSomeMnemonic )
import Codec.Binary.Bech32
    ( HumanReadablePart, humanReadablePartToText )
import Codec.Binary.Encoding
    ( AbstractEncoding (..)
    , Encoding
    , detectEncoding
    , encode
    , fromBase16
    , fromBase58
    , fromBech32
    )
import Control.Exception
    ( IOException )
import Control.Monad
    ( when )
import Data.ByteString
    ( ByteString )
import Data.List
    ( nub, sort )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , setSGRCode
    )
import System.Environment
    ( getProgName )
import System.Exit
    ( exitFailure )
import System.IO
    ( Handle, stderr )
import System.IO.Unsafe
    ( unsafePerformIO )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


--
-- I/O Read
--

-- | Read some bytes from the console, and decode them if the encoding is recognized.
hGetBytes :: Handle -> IO ByteString
hGetBytes h = do
    raw <- B8.filter noNewline <$> B8.hGetContents h
    case detectEncoding (T.unpack $ T.decodeUtf8 raw) of
        Just (EBase16  ) -> decode fromBase16 raw
        Just (EBech32{}) -> decode (fmap snd . fromBech32 markCharsRedAtIndices) raw
        Just (EBase58  ) -> decode fromBase58 raw
        Nothing          -> fail
            "Couldn't detect input encoding? Data on stdin must be encoded as \
            \bech16, bech32 or base58."
  where
    decode :: (bin -> Either String result) -> bin -> IO result
    decode from = either fail pure . from

-- | Read some bytes encoded in Bech32, only allowing the given prefixes.
hGetBech32 :: Handle -> [HumanReadablePart] -> IO (HumanReadablePart, ByteString)
hGetBech32 h allowedPrefixes = do
    raw <- B8.filter noNewline <$> B8.hGetContents h
    (hrp, bytes) <- decode (fromBech32 markCharsRedAtIndices) raw
    when (hrp `notElem` allowedPrefixes) $ fail
        $ "Invalid human-readable prefix. Prefix ought to be one of: "
        <> show (showHrp <$> allowedPrefixes)
    pure (hrp, bytes)
  where
    decode :: (bin -> Either String result) -> bin -> IO result
    decode from = either fail pure . from

    showHrp :: HumanReadablePart -> String
    showHrp = T.unpack . humanReadablePartToText

-- | Read some English mnemonic words from the console, or fail.
hGetSomeMnemonic :: Handle -> IO SomeMnemonic
hGetSomeMnemonic h = do
    wrds <- T.words . T.filter noNewline . T.decodeUtf8 <$> B8.hGetContents h
    case mkSomeMnemonic @'[ 9, 12, 15, 18, 21, 24 ] wrds of
        Left (MkSomeMnemonicError e) -> fail e
        Right mw -> pure mw

-- | Read an encoded private key from the console, or fail.
hGetXPrv :: Handle -> [HumanReadablePart] -> IO (HumanReadablePart, XPrv)
hGetXPrv h allowedPrefixes = do
    (hrp, bytes) <- hGetBech32 h allowedPrefixes
    case xprvFromBytes bytes of
        Nothing  -> fail "Couldn't convert bytes into extended private key."
        Just key -> pure (hrp, key)

-- | Read an encoded public key from the console, or fail.
hGetXPub :: Handle -> [HumanReadablePart] -> IO (HumanReadablePart, XPub)
hGetXPub h allowedPrefixes = do
    (hrp, bytes) <- hGetBech32 h allowedPrefixes
    case xpubFromBytes bytes of
        Nothing  -> fail "Couldn't convert bytes into extended public key."
        Just key -> pure (hrp, key)

-- | Read a script hash from the console, or fail.
hGetScriptHash :: Handle -> IO ScriptHash
hGetScriptHash h = do
    bytes <- hGetBytes h
    case scriptHashFromBytes bytes of
        Nothing  -> fail "Couldn't convert bytes into script hash."
        Just scriptHash -> pure scriptHash

-- | Read either an encoded public or private key from the console, or fail.
hGetXP__
    :: Handle
    -> [HumanReadablePart]
    -> IO (Either (HumanReadablePart, XPub) (HumanReadablePart, XPrv))
hGetXP__ h allowedPrefixes = do
    (hrp, bytes) <- hGetBech32 h allowedPrefixes
    case (xpubFromBytes bytes, xprvFromBytes bytes) of
        (Just xpub,         _) -> pure (Left  (hrp, xpub))
        (_        , Just xprv) -> pure (Right (hrp, xprv))
        _                      -> fail
            "Couldn't convert bytes into neither extended public or private keys."


--
-- I/O Write
--

-- | Print bytes to the console with the given encoding.
hPutBytes :: Handle -> ByteString -> Encoding -> IO ()
hPutBytes h bytes =
    B8.hPutStr h . flip encode bytes

--
-- Helpers
--

noNewline :: Char -> Bool
noNewline = (`notElem` ['\n', '\r'])

-- | Fail with a colored red error message.
prettyIOException :: IOException -> IO a
prettyIOException e = do
    B8.hPutStrLn stderr $ T.encodeUtf8 $ T.pack $ show e
    exitFailure

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

-- | Get program name to avoid hard-coding it in documentation excerpt.
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}
