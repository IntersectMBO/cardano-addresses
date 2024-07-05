{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Options.Applicative.Discrimination
    (
    -- * Type (re-export from Cardano.Address)
      NetworkTag(..)
    , fromNetworkTag

    -- * Applicative Parser
    , networkTagOpt
    ) where

import Prelude

import Cardano.Address
    ( NetworkDiscriminant (..), NetworkTag (..) )
import Cardano.Address.Style.Shelley
    ( Shelley )
import Data.List
    ( intercalate )
import Options.Applicative
    ( Parser
    , completer
    , eitherReader
    , helpDoc
    , listCompleter
    , long
    , metavar
    , option
    , (<|>)
    )
import Options.Applicative.Help.Pretty.Compat
    ( string, vsep )
import Options.Applicative.Style
    ( Style (..) )
import Text.Read
    ( readMaybe )

import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Shelley as Shelley

-- | Construct a Shelley 'NetworkDiscriminant' from a network tag. Fails loudly
-- if not possible.
fromNetworkTag :: MonadFail m => NetworkTag -> m (NetworkDiscriminant Shelley)
fromNetworkTag tag =
    case (Shelley.mkNetworkDiscriminant . fromIntegral . unNetworkTag) tag of
        Left Shelley.ErrWrongNetworkTag{} -> do
            fail "Invalid network tag. Must be between [0, 15]"
        Right discriminant ->
            pure discriminant

--
-- Applicative Parser
--

-- | Parse a 'NetworkTag' from the command-line, as an option
networkTagOpt :: Style -> Parser NetworkTag
networkTagOpt style = option (eitherReader reader) $ mempty
    <> metavar "NETWORK-TAG"
    <> long "network-tag"
    <> helpDoc  (Just (vsep (string <$> doc style)))
    <> completer (listCompleter $ show <$> tagsFor style)
  where
    doc style' =
        [ "A tag which identifies a Cardano network."
        , ""
        , header
        ]
        ++ (fmtAllowedKeyword <$> ("" : allowedKeywords style'))
        ++
        [ ""
        , "...or alternatively, an explicit network tag as an integer."
        ]
      where
        header = case style' of
            Byron ->
                "┌ Byron / Icarus ──────────"
            Icarus ->
                "┌ Byron / Icarus ──────────"
            Shelley ->
                "┌ Shelley ─────────────────"
            Shared ->
                "┌ Shared ──────────────────"
        fmtAllowedKeyword network =
            "│ " <> network

    tagsFor = \case
        Byron ->
            [ unNetworkTag (snd Byron.byronMainnet)
            , unNetworkTag (snd Byron.byronStaging)
            , unNetworkTag (snd Byron.byronTestnet)
            , unNetworkTag (snd Byron.byronPreprod)
            , unNetworkTag (snd Byron.byronPreview)
            ]
        Icarus ->
            tagsFor Byron
        Shelley ->
            [ unNetworkTag Shelley.shelleyMainnet
            , unNetworkTag Shelley.shelleyTestnet
            ]
        Shared ->
            [ unNetworkTag Shelley.shelleyMainnet
            , unNetworkTag Shelley.shelleyTestnet
            ]

    reader str = maybe (Left err) Right
        ((NetworkTag <$> readMaybe str) <|> (readKeywordMaybe str style))
      where
        err =
            "Invalid network tag. Must be an integer value or one of the \
            \allowed keywords: " <> intercalate ", " (allowedKeywords style)

    readKeywordMaybe str = \case
        Byron | str == "mainnet" -> pure (snd Byron.byronMainnet)
        Byron | str == "staging" -> pure (snd Byron.byronStaging)
        Byron | str == "testnet" -> pure (snd Byron.byronTestnet)
        Byron | str == "preview" -> pure (snd Byron.byronPreview)
        Byron | str == "preprod" -> pure (snd Byron.byronPreprod)
        Icarus -> readKeywordMaybe str Byron
        Shelley | str == "mainnet" -> pure Shelley.shelleyMainnet
        Shelley | str == "testnet" -> pure Shelley.shelleyTestnet
        Shelley | str == "preview" -> pure Shelley.shelleyTestnet
        Shelley | str == "preprod" -> pure Shelley.shelleyTestnet
        Shared | str == "mainnet" -> pure Shelley.shelleyMainnet
        Shared | str == "testnet" -> pure Shelley.shelleyTestnet
        Shared | str == "preview" -> pure Shelley.shelleyTestnet
        Shared | str == "preprod" -> pure Shelley.shelleyTestnet
        _ -> Nothing

    allowedKeywords = \case
        Byron -> ["mainnet", "staging", "testnet", "preview", "preprod"]
        Icarus -> allowedKeywords Byron
        Shelley -> ["mainnet", "testnet", "preview", "preprod"]
        Shared -> allowedKeywords Shelley
