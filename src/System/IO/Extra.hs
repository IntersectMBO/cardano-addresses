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
    , failWith
    , withSGR
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xprvFromBytes, xpubFromBytes )
import Cardano.Mnemonic
    ( MkSomeMnemonicError (..), SomeMnemonic, mkSomeMnemonic )
import Control.Applicative
    ( (<|>) )
import Control.Exception
    ( bracket )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Either.Extra
    ( eitherToMaybe )
import Options.Applicative.Encoding
    ( AbstractEncoding (..), Encoding )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , hSetSGR
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
    raw <- B8.hGetContents h
    case (tryBech32 raw <|> tryBase16 raw <|> tryBase58 raw) of
        Nothing    -> pure raw
        Just bytes -> pure bytes
  where
    tryBech32 raw = do
        (_hrp, dp) <- eitherToMaybe $ Bech32.decodeLenient (T.decodeUtf8 raw)
        Bech32.dataPartToBytes dp

    tryBase16 raw = do
        eitherToMaybe $ convertFromBase Base16 raw

    tryBase58 raw = do
        decodeBase58 bitcoinAlphabet raw

-- | Read some English mnemonic words from the console, or fail.
hGetSomeMnemonic :: Handle -> IO SomeMnemonic
hGetSomeMnemonic h = do
    wrds <- T.words . trim '\n' . T.decodeUtf8 <$> B8.hGetContents h
    case mkSomeMnemonic @'[ 9, 12, 15, 18, 21, 24 ] wrds of
        Left (MkSomeMnemonicError e) -> failWith e
        Right mw -> pure mw
  where
    trim c = T.filter (/= c)

-- | Read an encoded private key from the console, or fail.
hGetXPrv :: Handle -> IO XPrv
hGetXPrv h = do
    bytes <- hGetBytes h
    case xprvFromBytes bytes of
        Nothing  -> failWith "Couldn't convert bytes into extended private key."
        Just key -> pure key

-- | Read an encoded public key from the console, or fail.
hGetXPub :: Handle -> IO XPub
hGetXPub h = do
    bytes <- hGetBytes h
    case xpubFromBytes bytes of
        Nothing  -> failWith "Couldn't convert bytes into extended public key."
        Just key -> pure key

-- | Read either an encoded public or private key from the console, or fail.
hGetXP__ :: Handle -> IO (Either XPub XPrv)
hGetXP__ h = do
    bytes <- hGetBytes h
    case (xpubFromBytes bytes, xprvFromBytes bytes) of
        (Just xpub,         _) -> pure (Left  xpub)
        (_        , Just xprv) -> pure (Right xprv)
        _                      -> failWith
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
failWith :: String -> IO a
failWith msg = do
    withSGR stderr (SetColor Foreground Vivid Red) $ do
        B8.hPutStrLn stderr $ T.encodeUtf8 $ T.pack msg
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
