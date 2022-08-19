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
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Mnemonic
    ( SomeMnemonic, someMnemonicToBytes )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.Char
    ( toLower )
import Data.List
    ( intercalate )
import Data.Text
    ( Text )
import Options.Applicative
    ( Parser, argument, completer, eitherReader, help, listCompleter, metavar )

import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Address.Style.Shared as Shared
import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Data.ByteArray as BA
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
    FromMnemonic SomeMnemonic | FromText Text
    deriving (Eq, Show)

toSndFactor :: Maybe Passphrase -> ScrubbedBytes
toSndFactor = \case
    Nothing -> mempty
    Just (FromMnemonic mnemonic) -> someMnemonicToBytes mnemonic
    Just (FromText txt) -> BA.convert $ T.encodeUtf8 txt

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
        "shared"     -> Right Shared
        _             -> Left $ "Unknown style; expecting one of " <> styles'
