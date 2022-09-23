{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module System.IO.Extra
    (
    -- * I/O application-specific helpers
    -- ** Read
      hGetBytes
    , hGetBech32
    , hGetXPrv
    , hGetXPub
    , hGetXP__
    , hGetScriptHash
    , hGetSomeMnemonic
    , hGetSomeMnemonicInteractively
    , hGetPassphraseMnemonic
    , hGetPassphraseBytes

    -- ** Write
    , hPutBytes
    , hPutString

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
    , fromBase64
    , fromBech32
    )
import Control.Exception
    ( IOException, bracket )
import Control.Monad
    ( when )
import Data.ByteString
    ( ByteString )
import Data.List
    ( nub, sort )
import Data.Text
    ( Text )
import Data.Word
    ( Word8 )
import Options.Applicative.Style
    ( PassphraseInfo (..), PassphraseInputMode (..) )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , hCursorBackward
    , setSGRCode
    )
import System.Environment
    ( getProgName )
import System.Exit
    ( exitFailure )
import System.IO
    ( BufferMode (..)
    , Handle
    , hGetBuffering
    , hGetChar
    , hGetEcho
    , hPutChar
    , hSetBuffering
    , hSetEcho
    , stderr
    )
import System.IO.Unsafe
    ( unsafePerformIO )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

--
-- I/O Read
--

-- | Read some bytes from the console, and decode them if the encoding is recognized.
hGetBytes :: Handle -> IO ByteString
hGetBytes h = do
    raw <- B8.filter noNewline <$> B8.hGetContents h
    case detectEncoding (T.unpack $ T.decodeUtf8 raw) of
        Just (EBase16  ) -> decodeBytes fromBase16 raw
        Just (EBech32{}) -> decodeBytes (fmap snd . fromBech32 markCharsRedAtIndices) raw
        Just (EBase58  ) -> decodeBytes fromBase58 raw
        Nothing          -> fail
            "Couldn't detect input encoding? Data on stdin must be encoded as \
            \bech16, bech32 or base58."

decodeBytes
    :: (bin -> Either String result)
    -> bin
    -> IO result
decodeBytes from = either fail pure . from

-- | Read some bytes encoded in Bech32, only allowing the given prefixes.
hGetBech32 :: Handle -> [HumanReadablePart] -> IO (HumanReadablePart, ByteString)
hGetBech32 h allowedPrefixes = do
    raw <- B8.filter noNewline <$> B8.hGetContents h
    (hrp, bytes) <- decodeBytes (fromBech32 markCharsRedAtIndices) raw
    when (hrp `notElem` allowedPrefixes) $ fail
        $ "Invalid human-readable prefix. Prefix ought to be one of: "
        <> show (showHrp <$> allowedPrefixes)
    pure (hrp, bytes)
  where
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

withBuffering :: Handle -> BufferMode -> IO a -> IO a
withBuffering h buffering action = bracket aFirst aLast aBetween
  where
    aFirst = (hGetBuffering h <* hSetBuffering h buffering)
    aLast = hSetBuffering h
    aBetween = const action

withEcho :: Handle -> Bool -> IO a -> IO a
withEcho h echo action = bracket aFirst aLast aBetween
  where
    aFirst = (hGetEcho h <* hSetEcho h echo)
    aLast = hSetEcho h
    aBetween = const action

-- | Gather user inputs until a newline is met, hiding what's typed with a
-- placeholder character.
hGetSensitiveLine
    :: (Handle, Handle)
    -> PassphraseInputMode
    -> String
    -> IO Text
hGetSensitiveLine (hstdin, hstderr) mode prompt =
    withBuffering hstderr NoBuffering $
    withBuffering hstdin NoBuffering $
    withEcho hstdin False $ do
        hPutString hstderr prompt
        case mode of
            Sensitive ->
                getLineSensitive '*'
            Explicit ->
                getLineExplicit
  where
    backspace = toEnum 127

    getLineSensitive :: Char -> IO Text
    getLineSensitive placeholder =
        getLineSensitive' mempty
      where
        getLineSensitive' line = do
            hGetChar hstdin >>= \case
                '\n' -> do
                    hPutChar hstderr '\n'
                    return line
                c | c == backspace ->
                    if T.null line
                        then getLineSensitive' line
                        else do
                            hCursorBackward hstderr  1
                            hPutChar hstderr ' '
                            hCursorBackward hstderr 1
                            getLineSensitive' (T.init line)
                c -> do
                    hPutChar hstderr placeholder
                    getLineSensitive' (line <> T.singleton c)

    getLineExplicit :: IO Text
    getLineExplicit  =
        getLineExplicit' mempty
      where
        getLineExplicit' line = do
            hGetChar hstdin >>= \case
                '\n' -> do
                    hPutChar hstderr '\n'
                    return line
                c | c == backspace ->
                    if T.null line
                        then getLineExplicit' line
                        else do
                            hCursorBackward hstderr  1
                            hPutChar hstderr ' '
                            hCursorBackward hstderr 1
                            getLineExplicit' (T.init line)
                c -> do
                    getLineExplicit' (line <> T.singleton c)

-- | Prompt user and read some English mnemonic words from stdin.
hGetSomeMnemonicInteractively
    :: (Handle, Handle)
    -> PassphraseInputMode
    -> String
    -> IO SomeMnemonic
hGetSomeMnemonicInteractively (hstdin, hstderr) mode prompt = do
    wrds <- T.words . T.filter noNewline <$>
            hGetSensitiveLine (hstdin, hstderr) mode prompt
    case mkSomeMnemonic @'[ 9, 12, 15, 18, 21, 24 ] wrds of
        Left (MkSomeMnemonicError e) -> fail e
        Right mw -> pure mw

-- | Prompt user and read the mnemonic passphrase (second factor) from stdin.
hGetPassphraseMnemonic
    :: Handle
    -> IO SomeMnemonic
hGetPassphraseMnemonic h = do
    wrds <- T.words . T.filter noNewline . T.decodeUtf8 <$> BS.hGetLine h
    case mkSomeMnemonic @'[ 9, 12 ] wrds of
        Left (MkSomeMnemonicError e) -> fail e
        Right mw -> pure mw

-- | Read some bytes from the console, and decode them accoring to passphrase info.
hGetPassphraseBytes
    :: Handle
    -> PassphraseInfo
    -> IO ByteString
hGetPassphraseBytes h = \case
    Hex -> do
       raw <- B8.filter noNewline <$> B8.hGetLine h
       decodeBytes fromBase16 raw
    Base64 -> do
       raw <- B8.filter noNewline <$> B8.hGetLine h
       decodeBytes fromBase64 raw
    Utf8 -> do
       txt <- TIO.hGetLine h
       pure $ T.encodeUtf8 txt
    Octets -> do
       txt <- TIO.hGetLine h
       let bytes = read @[Word8] (T.unpack txt)
       pure $ BS.pack bytes
    _          -> fail
            "Data on stdin must be encoded as bech16, bech64, utf8 or octet array."

--
-- I/O Write
--

-- | Print bytes to the console with the given encoding.
hPutBytes :: Handle -> ByteString -> Encoding -> IO ()
hPutBytes h bytes =
    B8.hPutStr h . flip encode bytes

-- | Print string to the console.
hPutString :: Handle -> String -> IO ()
hPutString h =
    B8.hPutStrLn h . T.encodeUtf8 . T.pack

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
