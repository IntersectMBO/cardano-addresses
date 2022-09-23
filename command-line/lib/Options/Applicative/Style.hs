{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Options.Applicative.Style
    (
    -- * Type
      Style(..)
    , Passphrase (..)
    , PassphraseInfo (..)
    , PassphraseInputMode (..)
    , PassphraseInput (..)
    , generateRootKey

    -- * Applicative Parser
    , styleArg
    , passphraseInfoOpt
    , passphraseInputModeOpt
    , fileOpt
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Mnemonic
    ( SomeMnemonic, someMnemonicToBytes )
import Control.Applicative
    ( (<|>) )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( toLower )
import Data.List
    ( intercalate )
import Options.Applicative
    ( Parser
    , argument
    , completer
    , eitherReader
    , flag'
    , help
    , listCompleter
    , long
    , metavar
    , option
    )

import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Address.Style.Shared as Shared
import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Data.ByteArray as BA

--
-- Type
--

-- | Represent a style of wallet.
data Style
    = Byron
    | Icarus
    | Shelley
    | Shared
    deriving (Eq, Show, Enum, Bounded)

-- | User chosen passphrase for the generation phase
--
-- @since 3.13.0
data Passphrase =
    FromMnemonic SomeMnemonic | FromEncoded ByteString
    deriving (Eq, Show)

data PassphraseInfo =
    Mnemonic | Hex | Base64 | Utf8 | Octets
    deriving (Eq, Show)

data PassphraseInputMode =
    Sensitive | Silent | Explicit
    deriving (Eq, Show)

data PassphraseInput =
    Interactive | FromFile FilePath
    deriving (Eq, Show)

toSndFactor :: Maybe Passphrase -> ScrubbedBytes
toSndFactor = \case
    Nothing -> mempty
    Just (FromMnemonic mnemonic) -> someMnemonicToBytes mnemonic
    Just (FromEncoded bs) -> BA.convert bs

-- | Generate an extended root private key from a mnemonic sentence, in the
-- given style.
generateRootKey :: SomeMnemonic -> Maybe Passphrase -> Style -> IO XPrv
generateRootKey mw passwd = \case
    Byron -> do
        let rootK = Byron.genMasterKeyFromMnemonic mw
        pure $ Byron.getKey rootK
    Icarus -> do
        let sndFactor = toSndFactor passwd
        let rootK = Icarus.genMasterKeyFromMnemonic mw sndFactor
        pure $ Icarus.getKey rootK
    Shelley -> do
        let sndFactor = toSndFactor passwd
        let rootK = Shelley.genMasterKeyFromMnemonic mw sndFactor
        pure $ Shelley.getKey rootK
    Shared -> do
        let sndFactor = toSndFactor passwd
        let rootK = Shared.genMasterKeyFromMnemonic mw sndFactor
        pure $ Shared.getKey rootK

--
-- Applicative Parser
--

-- | Parse a 'Style' from the command-line, as an argument.
styleArg :: Parser Style
styleArg = argument (eitherReader reader) $ mempty
    <> metavar "STYLE"
    <> help styles'
    <> completer (listCompleter styles)
  where
    styles :: [String]
    styles = show @Style <$> [minBound .. maxBound]

    styles' = intercalate " | " styles

    reader :: String -> Either String Style
    reader str = case toLower <$> str of
        "byron"       -> Right Byron
        "icarus"      -> Right Icarus
        "shelley"     -> Right Shelley
        "shared"      -> Right Shared
        _             -> Left $ "Unknown style; expecting one of " <> styles'

passphraseInfoReader :: String -> Either String PassphraseInfo
passphraseInfoReader s = maybe (Left err) Right (readPassphraseInfoMaybe s)
  where
    err = "Invalid passphrase input type. Must be one of the \
          \allowed keywords: from-mnemonic, from-hex, from-base64, from-octets or from-utf8."
    readPassphraseInfoMaybe str
        | str == mempty          = pure Utf8
        | str == "from-mnemonic" = pure Mnemonic
        | str == "from-hex"      = pure Hex
        | str == "from-base64"   = pure Base64
        | str == "from-utf8"     = pure Utf8
        | str == "from-octets"   = pure Octets
        | otherwise              = Nothing

passphraseInfoOpt :: Parser PassphraseInfo
passphraseInfoOpt = option (eitherReader passphraseInfoReader) $ mempty
    <> long "passphrase"
    <> metavar "FORMAT"
    <> help helpDoc
  where
    helpDoc =
        "User chosen passphrase to be read from stdin for the generation phase. " ++
        "Valid for Icarus, Shelley and Shared styles. Accepting mnemonic " ++
        "(9- or 12 words) or arbitrary passphrase encoded as base16, base64, plain utf8 " ++
        "or raw bytes in the form of octet array."

-- | Parse an 'PassphraseInputMode' from the command-line, if there is proper flag then sensitive is set.
passphraseInputModeOpt :: Parser PassphraseInputMode
passphraseInputModeOpt = sensitive <|> silent <|> pure Explicit
  where
    sensitive = flag' Sensitive (long "sensitive")
    silent = flag' Silent (long "silent")

fileOpt :: Parser FilePath
fileOpt = option (eitherReader (\s ->  Right s)) $ mempty
   <> long "from-file"
   <> metavar "FILE"
   <> help ("Passphrase from specified filepath.")
