{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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

    -- * Derivation Scheme
    , DerivationScheme
    , derivationSchemeOpt
    ) where

import Prelude

import Cardano.Address.Derivation
    ( DerivationScheme (..), DerivationType (..), Index )
import Data.List
    ( intercalate, isSuffixOf )
import Data.Word
    ( Word32 )
import Options.Applicative
    ( Parser, argument, eitherReader, flag, help, long, metavar )
import Safe
    ( readEitherSafe )

import qualified Data.Text as T


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
        \'H' (44H/1815H/0H/0)."

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
        ix <- readEitherSafe txt
        mkDerivationIndex $ ix + indexToInteger firstHardened

    parseSoftIndex txt = do
        ix <- readEitherSafe txt
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

-- | Convert a 'DerivationIndex' back to string.
derivationIndexToString :: DerivationIndex -> String
derivationIndexToString ix_@(DerivationIndex ix)
    | ix_ >= firstHardened = show ix ++ "H"
    | otherwise            = show ix


--
-- DerivationScheme
--

-- | Parse a 'DerivationScheme' from the command-line as an optional flag.
derivationSchemeOpt :: Parser DerivationScheme
derivationSchemeOpt =
    flag DerivationScheme2 DerivationScheme1 (long "legacy")
