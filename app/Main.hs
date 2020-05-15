{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Main where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, xprvToBytes )
import Cardano.Mnemonic
    ( MkSomeMnemonicError (..)
    , Mnemonic
    , SomeMnemonic
    , entropyToMnemonic
    , genEntropy
    , mkSomeMnemonic
    , mnemonicToEntropy
    , mnemonicToText
    )
import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.Applicative
    ( (<|>) )
import Control.Exception
    ( bracket )
import Control.Monad
    ( guard, void )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Char
    ( toLower )
import Data.List
    ( intercalate )
import Data.List.Extra
    ( enumerate )
import Data.Maybe
    ( isNothing )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import Options.Applicative
    ( ArgumentFields
    , CommandFields
    , Mod
    , Parser
    , ParserInfo
    , argument
    , auto
    , command
    , customExecParser
    , eitherReader
    , flag
    , flag'
    , footerDoc
    , header
    , headerDoc
    , help
    , helper
    , info
    , long
    , maybeReader
    , metavar
    , option
    , prefs
    , progDesc
    , showDefaultWith
    , showHelpOnEmpty
    , subparser
    , value
    )
import Options.Applicative.Help.Pretty
    ( hardline, indent, string, vsep )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , hSetSGR
    , hSupportsANSIWithoutEmulation
    )
import System.Exit
    ( exitFailure )
import System.IO
    ( BufferMode (..)
    , Handle
    , hIsTerminalDevice
    , hSetBuffering
    , stderr
    , stdin
    , stdout
    )

import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Address.Style.Jormungandr as Jormungandr
import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = setup >> parseCmd >>= runCmd
  where
    parseCmd :: IO Cmd
    parseCmd = customExecParser (prefs showHelpOnEmpty) cmd

    -- Enable ANSI colors on Windows.
    setup :: IO ()
    setup = do
        void $ hSupportsANSIWithoutEmulation stderr
        hSetBuffering stdout NoBuffering
        hSetBuffering stderr NoBuffering

--
-- Command
--

data Cmd
    = CKey CmdKey
    | CMnemonic CmdMnemonic
    deriving (Show)

cmd :: ParserInfo Cmd
cmd = info (helper <*> parser) $ progDesc "cardano-addresses"
  where
    parser = subparser $ mconcat
        [ modMnemonic
        , modKey
        ]

runCmd :: Cmd -> IO ()
runCmd = \case
    CMnemonic sub -> runMnemonic sub
    CKey sub -> runKey sub

--
-- mnemonic
--
newtype CmdMnemonic
    = CGenerate CmdMnemonicGenerate
    deriving (Show)

modMnemonic :: Mod CommandFields Cmd
modMnemonic = command "mnemonic" $
    info (helper <*> fmap CMnemonic parser) mempty
  where
    parser = subparser $ mconcat
        [ modMnemonicGenerate
        ]

runMnemonic :: CmdMnemonic -> IO ()
runMnemonic = \case
    CGenerate sub -> runMnemonicGenerate sub

--
-- mnemonic generate
--

newtype CmdMnemonicGenerate = CmdMnemonicGenerate
    { size :: MnemonicSize
    } deriving (Show)

modMnemonicGenerate :: Mod CommandFields CmdMnemonic
modMnemonicGenerate = command "generate" $
    info (helper <*> fmap CGenerate parser) mempty
  where
    parser = CmdMnemonicGenerate
        <$> mnemonicSizeOpt

runMnemonicGenerate :: CmdMnemonicGenerate -> IO ()
runMnemonicGenerate CmdMnemonicGenerate{size} = do
    m <- case size of
        MS_9  -> mnemonicToText @9  . entropyToMnemonic <$> genEntropy
        MS_12 -> mnemonicToText @12 . entropyToMnemonic <$> genEntropy
        MS_15 -> mnemonicToText @15 . entropyToMnemonic <$> genEntropy
        MS_18 -> mnemonicToText @18 . entropyToMnemonic <$> genEntropy
        MS_21 -> mnemonicToText @21 . entropyToMnemonic <$> genEntropy
        MS_24 -> mnemonicToText @24 . entropyToMnemonic <$> genEntropy
    BS.putStrLn $ T.encodeUtf8 $ T.unwords m

--
-- key
--

data CmdKey
    = CFromMnemonic CmdKeyFromMnemonic
    deriving (Show)

modKey :: Mod CommandFields Cmd
modKey = command "key" $
    info (helper <*> fmap CKey parser) $ mempty
        <> progDesc "Derive and manipulate keys."
        <> footerDoc (Just $ string $ mconcat
            [ "Keys are read from standard input for convenient chaining of commands."
            , "\n\n"
            , "Multiple input and output encodings are supported. Input encodings "
            , "are automatically recognized whereas output encodings can be "
            , "tweaked via command-line options."
            , "\n\n"
            , "Example:\n\n"
            , "  $ cardano-address mnemonic generate \\\n"
            , "  | cardano-address key from-mnemonic icarus \\\n"
            , "  | cardano-address key public\n"
            , "  xpub1k365denpkmqhj9zj6qpax..."
            ])
  where
    parser = subparser $ mconcat
        [ modKeyFromMnemonic
        ]

runKey :: CmdKey -> IO ()
runKey = \case
    CFromMnemonic sub -> runKeyFromMnemonic sub

--
-- key from-mnemonic
--

data CmdKeyFromMnemonic = CmdKeyFromMnemonic
    { encoding :: Encoding
    , style :: Style
    } deriving (Show)

modKeyFromMnemonic :: Mod CommandFields CmdKey
modKeyFromMnemonic = command "from-mnemonic" $
    info (helper <*> fmap CFromMnemonic parser) $ mempty
        <> progDesc "Generate a root extended private key from a mnemonic sentence."
  where
    parser = CmdKeyFromMnemonic
        <$> encodingOpt [humanReadablePart|xprv|]
        <*> styleArg

runKeyFromMnemonic :: CmdKeyFromMnemonic -> IO ()
runKeyFromMnemonic CmdKeyFromMnemonic{encoding,style} = do
    someMnemonic <- hGetSomeMnemonic stdin
    rootK <- generateRootKey someMnemonic style
    hPutBytes stdout (xprvToBytes rootK) encoding

--
-- Style
--

data Style
    = Byron
    | Icarus
    | Jormungandr
    | Shelley
    deriving (Eq, Show)

styleArg :: Parser Style
styleArg = argument (maybeReader reader) $ mempty
    <> metavar "STYLE"
    <> help "Byron | Icarus | Jormungandr | Shelley"
  where
    reader :: String -> Maybe Style
    reader str = case toLower <$> str of
        "byron"       -> Just Byron
        "icarus"      -> Just Icarus
        "jormungandr" -> Just Jormungandr
        "shelley"     -> Just Shelley
        _             -> Nothing

-- TODO
-- Allow passphrase here.
generateRootKey :: SomeMnemonic -> Style -> IO XPrv
generateRootKey mw = \case
    Byron -> do
        let rootK = Byron.genMasterKeyFromMnemonic mw
        pure $ Byron.getKey rootK
    Icarus -> do
        let sndFactor = mempty
        let rootK = Icarus.genMasterKeyFromMnemonic mw sndFactor
        pure $ Icarus.getKey rootK
    Jormungandr -> do
        let sndFactor = mempty
        let rootK = Jormungandr.genMasterKeyFromMnemonic mw sndFactor
        pure $ Jormungandr.getKey rootK
    Shelley -> do
        let sndFactor = mempty
        let rootK = Shelley.genMasterKeyFromMnemonic mw sndFactor
        pure $ Shelley.getKey rootK

--
-- Encoding
--

data Encoding
    = EBase16
    | EBase58
    | EBech32 HumanReadablePart
    deriving (Eq, Show)

encodingOpt :: HumanReadablePart -> Parser Encoding
encodingOpt hrp = base16Flag <|> base58Flag <|> bech32Flag
  where
    base16Flag = flag'  EBase16      (long "base16")
    base58Flag = flag'  EBase58      (long "base58")
    bech32Flag = flag' (EBech32 hrp) (long "bech32")

--
-- MnemonicSize
--

data MnemonicSize
    = MS_9 | MS_12 | MS_15 | MS_18 | MS_21 | MS_24
    deriving (Generic, Show, Bounded, Enum, Eq)

mnemonicSizeOpt :: Parser MnemonicSize
mnemonicSizeOpt = option (eitherReader mnemonicSizeFromString) $ mempty
    <> long "size"
    <> metavar "INT"
    <> help "number of mnemonic words to generate."
    <> value MS_15
    <> showDefaultWith mnemonicSizeToString

mnemonicSizeToString :: MnemonicSize -> String
mnemonicSizeToString = drop 3 . show

mnemonicSizeFromString :: String -> Either String MnemonicSize
mnemonicSizeFromString str =
    case lookup str sizeMap of
        Just ms -> Right ms
        Nothing -> Left $ mempty
            <> "Invalid mnemonic size. Expected one of: "
            <> intercalate ", " sizeStrs
            <> "."
  where
    sizes    = enumerate
    sizeMap  = sizeStrs `zip` sizes
    sizeStrs = mnemonicSizeToString <$> sizes

--
-- I/O Helpers
--

-- Read some English mnemonic words from stdin, or fail.
hGetSomeMnemonic :: Handle -> IO SomeMnemonic
hGetSomeMnemonic h = do
    wrds <- T.words . trim '\n' . T.decodeUtf8 <$> BS.hGetContents h
    case mkSomeMnemonic @'[ 9, 12, 15, 18, 21, 24 ] wrds of
        Left (MkSomeMnemonicError e) -> failWith e
        Right mw -> pure mw
  where
    trim c = T.filter (/= c)

-- Print bytes to the console with the given encoding.
hPutBytes :: Handle -> ByteString -> Encoding -> IO ()
hPutBytes h bytes = \case
    EBase16 ->
        BS.hPutStr h $ convertToBase Base16 bytes
    EBase58 ->
        BS.hPutStr h $ encodeBase58 bitcoinAlphabet bytes
    EBech32 hrp ->
        BS.hPutStr h $ T.encodeUtf8 $ Bech32.encodeLenient hrp $ Bech32.dataPartFromBytes bytes

-- | Fail with a colored red error message.
failWith :: String -> IO a
failWith msg = do
    withSGR stderr (SetColor Foreground Vivid Red) $ do
        BS.hPutStrLn stderr $ T.encodeUtf8 $ T.pack msg
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
