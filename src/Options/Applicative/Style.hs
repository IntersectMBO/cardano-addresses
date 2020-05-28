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
    , encodeWithStyle
    -- , decodeWithStyle

    -- * Applicative Parser
    , styleArg
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Data.Binary.Put
    ( Put, putStringUtf8, runPut )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( toLower )
import Data.List
    ( intercalate )
import Options.Applicative
    ( Parser, argument, eitherReader, help, metavar )

import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Address.Style.Jormungandr as Jormungandr
import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

--
-- Type
--

-- | Represent a style of wallet.
data Style
    = Byron
    | Icarus
    | Jormungandr
    | Shelley
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
    Jormungandr -> do
        let sndFactor = mempty
        let rootK = Jormungandr.genMasterKeyFromMnemonic mw sndFactor
        pure $ Jormungandr.getKey rootK
    Shelley -> do
        let sndFactor = mempty
        let rootK = Shelley.genMasterKeyFromMnemonic mw sndFactor
        pure $ Shelley.getKey rootK

-- | Encode some data, with style (っ▀¯▀)つ
--
-- The resulting output is padded to be at least 16-byte long.
--
-- NOTE
-- The command-line automatically detect the encoding from inputs it read on
-- stdin. To minimize possible collisions on short intermediate representation
-- we add an arbitrary padding so that there's no (reduced) risk that a bech32
-- encoded string would be interpreted as base58 or vice-versa.
encodeWithStyle :: Style -> Put -> ByteString
encodeWithStyle style embed = padLeft 16 $ BL.toStrict $ runPut $ do
    putStringUtf8 (show style)
    embed
  where
    padLeft :: Int -> ByteString -> ByteString
    padLeft n bytes
        | BS.length bytes >= n = bytes
        | otherwise            = BS.replicate (n - BS.length bytes) 0 <> bytes

--
-- Applicative Parser
--

-- | Parse a 'Style' from the command-line, as an argument.
styleArg :: Parser Style
styleArg = argument (eitherReader reader) $ mempty
    <> metavar "STYLE"
    <> help styles
  where
    styles :: String
    styles = intercalate " | " $ show @Style <$> [minBound .. maxBound]

    reader :: String -> Either String Style
    reader str = case toLower <$> str of
        "byron"       -> Right Byron
        "icarus"      -> Right Icarus
        "jormungandr" -> Right Jormungandr
        "shelley"     -> Right Shelley
        _             -> Left $ "Unknown style; expecting one of " <> styles
