{-# LANGUAGE DataKinds #-}
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
    , hPutBytes

    -- * I/O Helpers
    , prettyIOException
    , progName
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xprvFromBytes, xpubFromBytes )
import Cardano.Mnemonic
    ( MkSomeMnemonicError (..), SomeMnemonic, mkSomeMnemonic )
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
import Data.ByteString
    ( ByteString )
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
    raw <- B8.filter isNewline <$> B8.hGetContents h
    case detectEncoding (T.unpack $ T.decodeUtf8 raw) of
        Just (EBase16  ) -> decode fromBase16 raw
        Just (EBech32{}) -> decode fromBech32 raw
        Just (EBase58  ) -> decode fromBase58 raw
        Nothing          -> fail
            "Couldn't detect input encoding? Data on stdin must be encoded as \
            \bech16, bech32 or base58."
  where
    decode :: (bin -> Either String bin) -> bin -> IO bin
    decode from = either fail pure . from

-- | Read some English mnemonic words from the console, or fail.
hGetSomeMnemonic :: Handle -> IO SomeMnemonic
hGetSomeMnemonic h = do
    wrds <- T.words . T.filter isNewline . T.decodeUtf8 <$> B8.hGetContents h
    case mkSomeMnemonic @'[ 9, 12, 15, 18, 21, 24 ] wrds of
        Left (MkSomeMnemonicError e) -> fail e
        Right mw -> pure mw

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
hPutBytes h bytes =
    B8.hPutStr h . flip encode bytes

--
-- Helpers
--

isNewline :: Char -> Bool
isNewline = (`notElem` ['\n', '\r'])

-- | Fail with a colored red error message.
prettyIOException :: IOException -> IO a
prettyIOException e = do
    B8.hPutStrLn stderr $ T.encodeUtf8 $ T.pack $ show e
    exitFailure

-- | Get program name to avoid hard-coding it in documentation excerpt.
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}
