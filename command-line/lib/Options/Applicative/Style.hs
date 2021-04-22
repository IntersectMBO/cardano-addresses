{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Options.Applicative.Style
    (
    -- * Type
      Style(..)
    , generateRootKey

    -- * Applicative Parser
    , styleArg
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Data.Char
    ( toLower )
import Data.List
    ( intercalate )
import Options.Applicative
    ( Parser, argument, completer, eitherReader, help, listCompleter, metavar )

import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Address.Style.Shared as Shared
import qualified Cardano.Address.Style.Shelley as Shelley

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

-- TODO
-- Allow passphrase here.
--
-- | Generate an extended root private key from a mnemonic sentence, in the
-- given style.
generateRootKey :: SomeMnemonic -> Style -> IO XPrv
generateRootKey mw = \case
    Byron -> do
        let rootK = Byron.genMasterKeyFromMnemonic mw
        pure $ Byron.getKey rootK
    Icarus -> do
        let sndFactor = mempty
        let rootK = Icarus.genMasterKeyFromMnemonic mw sndFactor
        pure $ Icarus.getKey rootK
    Shelley -> do
        let sndFactor = mempty
        let rootK = Shelley.genMasterKeyFromMnemonic mw sndFactor
        pure $ Shelley.getKey rootK
    Shared -> do
        let sndFactor = mempty
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
