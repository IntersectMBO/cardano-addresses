{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Derivation
    (
    -- * Derivation Path
    -- ** Type
      DerivationPath
    , castDerivationPath
    , derivationPathToString
    , derivationPathFromString
    -- ** Applicative Parser
    , derivationPathArg

    -- * Derivation Index
    , DerivationIndex
    , mkDerivationIndex
    , firstHardened
    , indexToInteger
    , derivationIndexToString
    , derivationIndexFromString

    -- * XPub / XPrv
    , xpubOpt
    , xpubArg

    -- * Internal
    , encodingReader
    ) where

import Prelude

import Cardano.Address.Derivation
    ( DerivationType (..), Index, XPub, xpubFromBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..)
    , detectEncoding
    , fromBase16
    , fromBase58
    , fromBech32
    )
import Control.Arrow
    ( left )
import Data.ByteString
    ( ByteString )
import Data.List
    ( intercalate, isSuffixOf )
import Data.Word
    ( Word32 )
import Options.Applicative
    ( Parser
    , argument
    , completer
    , eitherReader
    , help
    , listCompleter
    , long
    , metavar
    , option
    )
import Safe
    ( readEitherSafe )
import System.IO.Extra
    ( markCharsRedAtIndices )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T


--
-- Derivation Path
--

-- | Represent a user-provided 'DerivationPath'.
newtype DerivationPath = DerivationPath [DerivationIndex]
    deriving (Show, Eq)

derivationPathFromString :: String -> Either String DerivationPath
derivationPathFromString str =
    DerivationPath
        <$> mapM (derivationIndexFromString . T.unpack) (T.splitOn "/" txt)
  where
    txt = T.pack str

derivationPathToString :: DerivationPath -> String
derivationPathToString (DerivationPath xs) =
    intercalate "/" $ map derivationIndexToString xs

castDerivationPath :: DerivationPath -> [Index 'WholeDomain depth]
castDerivationPath (DerivationPath xs) = toEnum . fromEnum <$> xs

derivationPathArg :: Parser DerivationPath
derivationPathArg = argument (eitherReader derivationPathFromString) $ mempty
    <> metavar "DERIVATION-PATH"
    <> help
        "Slash-separated derivation path. Hardened indexes are marked with a \
        \'H' (e.g. 1852H/1815H/0H/0)."
    <> completer (listCompleter ["1852H/1815H/0H/", "44H/1815H/0H/"])

--
-- Derivation Index
--

newtype DerivationIndex = DerivationIndex Word32
    deriving stock   (Show, Eq)
    deriving newtype (Bounded, Enum, Ord)

-- | Safely cast a 'DerivationIndex' to an 'Integer'.
indexToInteger :: DerivationIndex -> Integer
indexToInteger (DerivationIndex ix) = fromIntegral ix

-- | Get the first 'DerivationIndex' considered /hardened/.
firstHardened :: DerivationIndex
firstHardened = DerivationIndex 0x80000000

-- | Smart-constructor for a 'DerivationIndex'
mkDerivationIndex :: Integer -> Either String DerivationIndex
mkDerivationIndex ix
    | ix > fromIntegral (maxBound @Word32) =
        Left $ show ix <> " is too high to be a derivation index."
    | otherwise =
        pure $ DerivationIndex $ fromIntegral ix

-- | Convert a string to a derivation index. String must be followed by a
-- capital /H/ to mark hardened index. For example @0@ refers to the first soft
-- index, whereas @0H@ refers to the first hardened index.
derivationIndexFromString :: String -> Either String DerivationIndex
derivationIndexFromString "" = Left "An empty string is not a derivation index!"
derivationIndexFromString str
    | "H" `isSuffixOf` str = do
        parseHardenedIndex (init str)
    | otherwise = do
        parseSoftIndex str
  where
    parseHardenedIndex txt = do
        ix <- left (const msg) $ readEitherSafe txt
        mkDerivationIndex $ ix + indexToInteger firstHardened
      where
        msg = mconcat
            [ "Unable to parse hardened index. Hardened indexes are integer "
            , "values, between "
            , show (indexToInteger (minBound @DerivationIndex))
            , " and "
            , show (indexToInteger firstHardened)
            , " ending with a capital 'H'. For example: \"42H\","
            ]

    parseSoftIndex txt = do
        ix <- left (const msg) $ readEitherSafe txt
        guardSoftIndex ix
        mkDerivationIndex ix
      where
        guardSoftIndex ix
            | ix >= indexToInteger firstHardened =
                Left $ mconcat
                    [ show ix
                    , " is too high to be a soft derivation index. "
                    , "Did you mean \""
                    , show (ix - indexToInteger firstHardened)
                    , "H\"?"
                    ]
            | otherwise =
                pure ()

        msg = mconcat
            [ "Unable to parse soft index. Soft indexes are integer "
            , "values, between "
            , show (indexToInteger (minBound @DerivationIndex))
            , " and "
            , show (indexToInteger firstHardened)
            , ". For example: \"14\"."
            ]

-- | Convert a 'DerivationIndex' back to string.
derivationIndexToString :: DerivationIndex -> String
derivationIndexToString ix_@(DerivationIndex ix)
    | ix_ >= firstHardened = show ix' ++ "H"
    | otherwise            = show ix
  where
    ix' = fromIntegral ix - indexToInteger firstHardened

encodingReader :: String -> Either String ByteString
encodingReader str = case detectEncoding str of
    Just EBase16   -> fromBase16 (toBytes str)
    Just EBech32{} -> snd <$> fromBech32 markCharsRedAtIndices (toBytes str)
    Just EBase58   -> fromBase58 (toBytes str)
    Nothing        -> Left
        "Couldn't detect input encoding? The key must be encoded as \
        \base16, bech32 or base58."
  where
    toBytes = T.encodeUtf8 . T.pack

xpubReader :: String -> Either String XPub
xpubReader str = do
    bytes <- encodingReader str
    case xpubFromBytes bytes of
        Just xpub -> pure xpub
        Nothing   -> Left
            "Failed to convert bytes into a valid extended public key."

xpubOpt :: String -> String -> Parser XPub
xpubOpt name helpDoc =
    option (eitherReader xpubReader) $ mempty
        <> long name
        <> metavar "XPUB"
        <> help helpDoc

xpubArg :: String -> Parser XPub
xpubArg helpDoc =
    argument (eitherReader xpubReader) $ mempty
        <> metavar "XPUB"
        <> help helpDoc
