{-# LANGUAGE FlexibleContexts #-}
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
        <> progDesc "About public/private keys"
        <> footerDoc (Just $ string $ mconcat
            [ "Example:\n\n"
            , "  $ cardano-address recovery-phrase generate --size 15 \\\n"
            , "  | cardano-address key from-recovery-phrase Shelley > root.prv \n"
            , "  \n"
            , "  $ cat root.prv \\\n"
            , "  | cardano-address key child 1852H/1815H/0H \\\n"
            , "  | tee acct.prv \\\n"
            , "  | cardano-address key public > acct.pub \n"
            , "  \n"
            , "  $ cat acct.prv | cardano-address key inspect\n"
            , "  key type:     private\n"
            , "  extended key: 586063c612e8eaa3d807c444c1e5c1630e5a486071edbe18fd900f68236d96438d5f4e3...\n"
            , "  chain code:   09b9b219ab1df2bce6597704009cc8ced2fccc9655ff0aebf409b55f0a36ee96\n"
            , "  \n"
            , "  $ cat acct.pub | cardano-address key inspect\n"
            , "  key type:     public\n"
            , "  extended key: 406f0caac3a0977684aa4b7182ff62612aa4fbe576c1ba0b9a41c7b11f4a1167\n"
            , "  chain code:   09b9b219ab1df2bce6597704009cc8ced2fccc9655ff0aebf409b55f0a36ee96\n"
            , "  \n"
            , "  $ cat acct.pub | cardano-address key child 0/0 > addr.pub"
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
