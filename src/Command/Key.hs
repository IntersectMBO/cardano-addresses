{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Options.Applicative
    ( CommandFields
    , Mod
    , command
    , footerDoc
    , helper
    , info
    , progDesc
    , subparser
    )
import Options.Applicative.Help.Pretty
    ( string )

import qualified Command.Key.Child as Child
import qualified Command.Key.FromRecoveryPhrase as FromRecoveryPhrase
import qualified Command.Key.Inspect as Inspect
import qualified Command.Key.Public as Public

data Cmd
    = FromRecoveryPhrase FromRecoveryPhrase.Cmd
    | Child Child.Cmd
    | Public Public.Cmd
    | Inspect Inspect.Cmd
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "key" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "About public/private keys."
        <> footerDoc (Just $ string $ mconcat
            [ "Example:\n"
            , "  $ cardano-address recovery-phrase generate \\\n"
            , "  | cardano-address key from-recovery-phrase icarus \\\n"
            , "  | cardano-address key public\n"
            , "  xpub1k365denpkmqhj9zj6qpax..."
            ])
  where
    parser = subparser $ mconcat
        [ FromRecoveryPhrase.mod FromRecoveryPhrase
        , Child.mod Child
        , Public.mod Public
        , Inspect.mod Inspect
        ]

run :: Cmd -> IO ()
run = \case
    FromRecoveryPhrase sub -> FromRecoveryPhrase.run sub
    Child sub -> Child.run sub
    Public sub -> Public.run sub
    Inspect sub -> Inspect.run sub
