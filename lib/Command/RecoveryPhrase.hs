{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Command.RecoveryPhrase
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc, subparser )
import Options.Applicative.Help.Pretty
    ( pretty, vsep )
import qualified Command.RecoveryPhrase.Generate as Generate


newtype Cmd
    = Generate Generate.Cmd
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "recovery-phrase" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Generate and work with recovery phrases"
        <> footerDoc (Just $ vsep
            [ pretty "Commands:"
            , pretty "  • generate - Generate a new recovery (mnemonic) phrase"
            , pretty ""
            , pretty "Examples:"
            , pretty "  Generate a 15-word English recovery phrase:"
            , pretty "    cardano-address recovery-phrase generate --size 15 --language en"
            , pretty ""
            , pretty "  Pipe to generate a root private key (Shelley style):"
            , pretty "    cardano-address recovery-phrase generate --size 24 \\"
            , pretty "      | cardano-address key from-recovery-phrase Shelley > root.xsk"
            ])
  where
    parser = subparser $ mconcat
        [ Generate.mod Generate
        ]

run :: Cmd -> IO ()
run = \case
    Generate sub -> Generate.run sub
