{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Options.Applicative.Discrimination
    (
    -- * Type (re-export from Cardano.Address)
      NetworkTag(..)

    -- * Applicative Parser
    , networkTagOpt
    ) where

import Prelude

import Cardano.Address
    ( NetworkTag (..) )
import Options.Applicative
    ( Parser, auto, helpDoc, long, metavar, option )
import Options.Applicative.Help.Pretty
    ( string, vsep )

import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Jormungandr as Jormungandr
import qualified Cardano.Address.Style.Shelley as Shelley

--
-- Applicative Parser
--

-- | Parse a 'NetworkTag' from the command-line, as an option
networkTagOpt :: Parser NetworkTag
networkTagOpt = option (NetworkTag <$> auto) $ mempty
    <> metavar "NETWORK-TAG"
    <> long "network-tag"
    <> helpDoc  (Just (vsep (string <$> doc)))
  where
    doc =
        [ "A tag which identifies a Cardano network."
        , ""
        , "┌ Byron / Icarus ──────────"
        , "│ mainnet: " <> show (unNetworkTag (snd Byron.byronMainnet))
        , "│ staging: " <> show (unNetworkTag (snd Byron.byronStaging))
        , "│ testnet: " <> show (unNetworkTag (snd Byron.byronTestnet))
        , ""
        , "┌ Jormungandr ─────────────"
        , "│ testnet: " <> show (unNetworkTag Jormungandr.incentivizedTestnet)
        , ""
        , "┌ Shelley ─────────────────"
        , "│ mainnet: " <> show (unNetworkTag Shelley.shelleyMainnet)
        , "│ testnet: " <> show (unNetworkTag Shelley.shelleyTestnet)
        , ""
        ]
