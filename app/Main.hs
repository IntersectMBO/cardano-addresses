{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    ( Depth (..)
    , DerivationScheme (..)
    , DerivationType (..)
    , Index
    , XPrv
    , XPub
    , deriveXPrv
    , deriveXPub
    , toXPub
    , xprvChainCode
    , xprvFromBytes
    , xprvPrivateKey
    , xprvToBytes
    , xpubChainCode
    , xpubFromBytes
    , xpubPublicKey
    , xpubToBytes
    )
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
    ( foldM, guard, void )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Char
    ( toLower )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Functor.Identity
    ( Identity (..) )
import Data.List
    ( intercalate, isSuffixOf )
import Data.List.Extra
    ( enumerate )
import Data.Maybe
    ( isNothing )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
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
import Safe
    ( readEitherSafe )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleIntensity (..)
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
    | CRecoveryPhrase CmdRecoveryPhrase
    deriving (Show)

cmd :: ParserInfo Cmd
cmd = info (helper <*> parser) $ progDesc "cardano-addresses"
  where
    parser = subparser $ mconcat
        [ modRecoveryPhrase
        , modKey
        ]

runCmd :: Cmd -> IO ()
runCmd = \case
    CRecoveryPhrase sub -> runRecoveryPhrase sub
    CKey sub -> runKey sub

--
-- mnemonic
--
newtype CmdRecoveryPhrase
    = CGenerate CmdRecoveryPhraseGenerate
    deriving (Show)

modRecoveryPhrase :: Mod CommandFields Cmd
modRecoveryPhrase = command "recovery-phrase" $
    info (helper <*> fmap CRecoveryPhrase parser) $ mempty
        <> progDesc "About recovery phrases."
  where
    parser = subparser $ mconcat
        [ modRecoveryPhraseGenerate
        ]

runRecoveryPhrase :: CmdRecoveryPhrase -> IO ()
runRecoveryPhrase = \case
    CGenerate sub -> runRecoveryPhraseGenerate sub

--
-- mnemonic generate
--

newtype CmdRecoveryPhraseGenerate = CmdRecoveryPhraseGenerate
    { size :: MnemonicSize
    } deriving (Show)

modRecoveryPhraseGenerate :: Mod CommandFields CmdRecoveryPhrase
modRecoveryPhraseGenerate = command "generate" $
    info (helper <*> fmap CGenerate parser) mempty
  where
    parser = CmdRecoveryPhraseGenerate
        <$> mnemonicSizeOpt

runRecoveryPhraseGenerate :: CmdRecoveryPhraseGenerate -> IO ()
runRecoveryPhraseGenerate CmdRecoveryPhraseGenerate{size} = do
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
    = CFromRecoveryPhrase CmdKeyFromRecoveryPhrase
    | CChild CmdKeyChild
    | CPublic CmdKeyPublic
    | CInspect CmdKeyInspect
    deriving (Show)

modKey :: Mod CommandFields Cmd
modKey = command "key" $
    info (helper <*> fmap CKey parser) $ mempty
        <> progDesc "About public/private keys."
        <> footerDoc (Just $ string $ mconcat
            [ "Example:\n"
            , "  $ cardano-address recovery-phrase generate \\\n"
            , "  | cardano-address key from-recovery-phrase icarus \\\n"
            , "  | cardano-address key public\n"
            , "  xpub1k365denpkmqhj9zj6qpax..."
            ])
  where
    parser = subparser $ mconcat
        [ modKeyFromRecoveryPhrase
        , modKeyChild
        , modKeyPublic
        , modKeyInspect
        ]

runKey :: CmdKey -> IO ()
runKey = \case
    CFromRecoveryPhrase sub -> runKeyFromRecoveryPhrase sub
    CChild sub -> runKeyChild sub
    CPublic sub -> runKeyPublic sub
    CInspect sub -> runKeyInspect sub

--
-- key from-recovery-phrase
--

data CmdKeyFromRecoveryPhrase = CmdKeyFromRecoveryPhrase
    { encoding :: Encoding
    , style :: Style
    } deriving (Show)

modKeyFromRecoveryPhrase :: Mod CommandFields CmdKey
modKeyFromRecoveryPhrase = command "from-recovery-phrase" $
    info (helper <*> fmap CFromRecoveryPhrase parser) $ mempty
        <> progDesc "Convert a recovery phrase to an extended private key."
        <> footerDoc (Just $ string $ mconcat
            [ "The recovery phrase is read from stdin."
            , "\n\n"
            , "Example:\n\n"
            , "  $ cardano-address recovery-phrase generate \\\n"
            , "  | cardano-address key from-recovery-phrase icarus \\\n"
            ])
  where
    parser = CmdKeyFromRecoveryPhrase
        <$> encodingOpt [humanReadablePart|xprv|]
        <*> styleArg

runKeyFromRecoveryPhrase :: CmdKeyFromRecoveryPhrase -> IO ()
runKeyFromRecoveryPhrase CmdKeyFromRecoveryPhrase{encoding,style} = do
    someMnemonic <- hGetSomeMnemonic stdin
    rootK <- generateRootKey someMnemonic style
    hPutBytes stdout (xprvToBytes rootK) encoding

--
-- key child
--

data CmdKeyChild = CmdKeyChild
    { encoding :: Encoding
    , scheme :: DerivationScheme
    , path :: DerivationPath
    } deriving (Show)

schemeOpt :: Parser DerivationScheme
schemeOpt = flag DerivationScheme2 DerivationScheme1 (long "legacy")

modKeyChild :: Mod CommandFields CmdKey
modKeyChild = command "child" $
    info (helper <*> fmap CChild parser) $ mempty
        <> progDesc "Derive child keys from a parent public/private key."
        <> footerDoc (Just $ string $ mconcat
            [ "The parent key is read from stdin."
            ])
  where
    parser = CmdKeyChild
        <$> encodingOpt [humanReadablePart|???|]
        <*> schemeOpt
        <*> derivationPathArg

runKeyChild :: CmdKeyChild -> IO ()
runKeyChild CmdKeyChild{encoding,path,scheme} = do
    xkey <- hGetXP__ stdin

    (child,encoding') <- case xkey of
        Left xpub -> do
            let ixs = castDerivationPath path
            case foldM (deriveXPub scheme) xpub ixs of
                Nothing ->
                    failWith
                        "Couldn't derive child key. If you're trying to derive \
                        \children on a PUBLIC key, you must use soft indexes only."
                Just child ->
                    pure
                        ( xpubToBytes child
                        , mapHRP (const [humanReadablePart|xpub|] ) encoding
                        )

        Right xprv -> do
            let ixs = castDerivationPath path
            let Identity child = foldM (\k -> pure . deriveXPrv scheme k) xprv ixs
            pure
                ( xprvToBytes child
                , mapHRP (const [humanReadablePart|xprv|]) encoding
                )

    hPutBytes stdout child encoding'

--
-- key public
--

newtype CmdKeyPublic = CmdKeyPublic
    { encoding :: Encoding
    } deriving (Show)

modKeyPublic :: Mod CommandFields CmdKey
modKeyPublic = command "public" $
    info (helper <*> fmap CPublic parser) $ mempty
        <> progDesc "Get the public counterpart of a private key."
        <> footerDoc (Just $ string $ mconcat
            [ "The private key is read from stdin."
            ])
  where
    parser = CmdKeyPublic
        <$> encodingOpt [humanReadablePart|xpub|]

runKeyPublic :: CmdKeyPublic -> IO ()
runKeyPublic CmdKeyPublic{encoding} = do
    xprv <- hGetXPrv stdin
    let xpub = toXPub xprv
    hPutBytes stdout (xpubToBytes xpub) encoding


--
-- key inspect
--

data CmdKeyInspect = CmdKeyInspect
    deriving (Show)

modKeyInspect :: Mod CommandFields CmdKey
modKeyInspect = command "inspect" $
    info (helper <*> fmap CInspect parser) $ mempty
        <> progDesc "Show information about a key."
        <> footerDoc (Just $ string $ mconcat
            [ "The parent key is read from stdin."
            ])
  where
    parser = pure CmdKeyInspect

runKeyInspect :: CmdKeyInspect -> IO ()
runKeyInspect CmdKeyInspect = do
    either inspectXPub inspectXPrv =<< hGetXP__ stdin
  where
    newline = BS.hPutStrLn stdout ""

    inspectXPub :: XPub -> IO ()
    inspectXPub xpub = do
        hPutBold stdout "key type:     " *> BS.hPutStr stdout "public"    *> newline
        hPutBold stdout "extended key: " *> hPutBytes  stdout pub EBase16 *> newline
        hPutBold stdout "chain code:   " *> hPutBytes  stdout cc  EBase16 *> newline
      where
        pub = xpubPublicKey xpub
        cc  = xpubChainCode xpub

    inspectXPrv :: XPrv -> IO ()
    inspectXPrv xprv = do
        hPutBold stdout "key type:     " *> BS.hPutStr stdout "private"   *> newline
        hPutBold stdout "extended key: " *> hPutBytes  stdout prv EBase16 *> newline
        hPutBold stdout "chain code:   " *> hPutBytes  stdout cc  EBase16 *> newline
      where
        prv = xprvPrivateKey xprv
        cc  = xprvChainCode  xprv

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
    base16Flag = flag'  EBase16 (long "base16")
    base58Flag = flag'  EBase58 (long "base58")
    bech32Flag = flag (EBech32 hrp) (EBech32 hrp) (long "bech32")

mapHRP :: (HumanReadablePart -> HumanReadablePart) -> Encoding -> Encoding
mapHRP fn = \case
    EBase16 -> EBase16
    EBase58 -> EBase58
    EBech32 hrp -> EBech32 (fn hrp)

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
-- Derivation Path
--

newtype DerivationPath = DerivationPath [DerivationIndex]
    deriving (Show, Eq)

derivationPathArg :: Parser DerivationPath
derivationPathArg = argument (eitherReader derivationPathFromString) $ mempty
    <> metavar "DERIVATION-PATH"
    <> help
        "Slash-separated derivation path. Hardened indexes are marked with a \
        \'H' (44H/1815H/0H/0)."

castDerivationPath ::DerivationPath -> [Index 'WholeDomain depth]
castDerivationPath (DerivationPath xs) = toEnum . fromEnum <$> xs

derivationPathFromString :: String -> Either String DerivationPath
derivationPathFromString str =
    DerivationPath
        <$> mapM (derivationIndexFromString . T.unpack) (T.splitOn "/" txt)
  where
    txt = T.pack str

derivationPathToString :: DerivationPath -> String
derivationPathToString (DerivationPath xs) =
    intercalate "/" $ map derivationIndexToString xs

--
-- Derivation Index
--

newtype DerivationIndex = DerivationIndex Word32
    deriving stock   (Show, Eq)
    deriving newtype (Bounded, Enum, Ord)

mkDerivationIndex :: Integer -> Either String DerivationIndex
mkDerivationIndex ix
    | ix > fromIntegral (maxBound @Word32) =
        Left $ show ix <> " is too high to be a derivation index."
    | otherwise =
        pure $ DerivationIndex $ fromIntegral ix

firstHardened :: Word32
firstHardened = 0x80000000

derivationIndexFromString :: String -> Either String DerivationIndex
derivationIndexFromString "" = Left "An empty string is not a derivation index!"
derivationIndexFromString str
    | "H" `isSuffixOf` str = do
        parseHardenedIndex (init str)
    | otherwise = do
        parseSoftIndex str
  where
    parseHardenedIndex txt = do
        ix <- readEitherSafe txt
        mkDerivationIndex $ ix + fromIntegral firstHardened

    parseSoftIndex txt = do
        ix <- readEitherSafe txt
        guardSoftIndex ix
        mkDerivationIndex ix
      where
        guardSoftIndex ix
            | ix >= fromIntegral firstHardened =
                Left $ mconcat
                    [ show ix
                    , " is too high to be a soft derivation index. "
                    , "Did you mean \""
                    , show (ix - fromIntegral firstHardened)
                    , "H\"?"
                    ]
            | otherwise =
                pure ()

derivationIndexToString :: DerivationIndex -> String
derivationIndexToString (DerivationIndex ix)
    | ix >= firstHardened = show ix ++ "H"
    | otherwise           = show ix

--
-- I/O Helpers
--

-- Read some English mnemonic words from the console, or fail.
hGetSomeMnemonic :: Handle -> IO SomeMnemonic
hGetSomeMnemonic h = do
    wrds <- T.words . trim '\n' . T.decodeUtf8 <$> BS.hGetContents h
    case mkSomeMnemonic @'[ 9, 12, 15, 18, 21, 24 ] wrds of
        Left (MkSomeMnemonicError e) -> failWith e
        Right mw -> pure mw
  where
    trim c = T.filter (/= c)

-- Read an encoded private key from the console, or fail.
hGetXPrv :: Handle -> IO XPrv
hGetXPrv h = do
    bytes <- hGetBytes h
    case xprvFromBytes bytes of
        Nothing  -> failWith "Couldn't convert bytes into extended private key."
        Just key -> pure key

-- Read an encoded public key from the console, or fail.
hGetXPub :: Handle -> IO XPub
hGetXPub h = do
    bytes <- hGetBytes h
    case xpubFromBytes bytes of
        Nothing  -> failWith "Couldn't convert bytes into extended public key."
        Just key -> pure key

-- Read either an encoded public or private key from the console, or fail.
hGetXP__ :: Handle -> IO (Either XPub XPrv)
hGetXP__ h = do
    bytes <- hGetBytes h
    case (xpubFromBytes bytes, xprvFromBytes bytes) of
        (Just xpub,         _) -> pure (Left  xpub)
        (_        , Just xprv) -> pure (Right xprv)
        _                      -> failWith
            "Couldn't convert bytes into neither extended public or private keys."

-- Print bytes to the console with the given encoding.
hPutBytes :: Handle -> ByteString -> Encoding -> IO ()
hPutBytes h bytes = \case
    EBase16 ->
        BS.hPutStr h $ convertToBase Base16 bytes
    EBase58 ->
        BS.hPutStr h $ encodeBase58 bitcoinAlphabet bytes
    EBech32 hrp ->
        BS.hPutStr h $ T.encodeUtf8 $ Bech32.encodeLenient hrp $ Bech32.dataPartFromBytes bytes

hPutBold :: Handle -> ByteString -> IO ()
hPutBold h bytes =
    withSGR h (SetConsoleIntensity BoldIntensity) $ BS.hPutStr h bytes

-- Read some bytes from the console, and decode them if the encoding is recognized.
hGetBytes :: Handle -> IO ByteString
hGetBytes h = do
    raw <- BS.hGetContents h
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
