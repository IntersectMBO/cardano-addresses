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
    , generateRootKey

    -- * Applicative Parser
    , styleArg
    , passphraseOpt
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Mnemonic
    ( SomeMnemonic, mkSomeMnemonic, someMnemonicToBytes )
import Codec.Binary.Encoding
    ( fromBase16, fromBase58 )
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
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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

passphraseReader :: String -> Either String Passphrase
passphraseReader str =
    if null str then
        pure $ FromEncoded mempty
    else do
        let wrds = T.words . T.filter noNewline . T.decodeUtf8 . B8.pack $ str
        case mkSomeMnemonic @'[ 9, 12 ] wrds of
            Right mw -> pure $ FromMnemonic mw
            Left _ -> getBase16 <|> getBase58
  where
    getBase16 = case fromBase16 (T.encodeUtf8 $ T.pack str) of
        Right hex -> pure $ FromEncoded hex
        Left _ -> Left "not hex-encoded bytes"
    getBase58 = case fromBase58 (T.encodeUtf8 $ T.pack str) of
        Right bs -> pure $ FromEncoded bs
        Left _ -> Left "not base58-encoded bytes"
    noNewline :: Char -> Bool
    noNewline = (`notElem` ['\n', '\r'])

passphraseOpt :: Parser Passphrase
passphraseOpt = option (eitherReader passphraseReader) $ mempty
    <> long "passphrase"
    <> metavar "PASSWORD"
    <> help helpDoc
  where
    helpDoc =
        "User chosen passphrase for the generation phase. Valid for Icarus, " ++
        "Shelley and Shared styles. Accept mnemonic (9- or 12-word length) or " ++
        "arbitrary passphrase encoded as base16 or base58."
