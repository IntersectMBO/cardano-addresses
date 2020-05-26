{-# LANGUAGE LambdaCase #-}

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
import Options.Applicative
    ( Parser, argument, help, maybeReader, metavar )

import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Address.Style.Jormungandr as Jormungandr
import qualified Cardano.Address.Style.Shelley as Shelley

--
-- Type
--

-- | Represent a style of wallet.
data Style
    = Byron
    | Icarus
    | Jormungandr
    | Shelley
    deriving (Eq, Show)

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
    Jormungandr -> do
        let sndFactor = mempty
        let rootK = Jormungandr.genMasterKeyFromMnemonic mw sndFactor
        pure $ Jormungandr.getKey rootK
    Shelley -> do
        let sndFactor = mempty
        let rootK = Shelley.genMasterKeyFromMnemonic mw sndFactor
        pure $ Shelley.getKey rootK

--
-- Applicative Parser
--

-- | Parse a 'Style' from the command-line, as an argument.
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
