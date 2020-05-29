{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.RecoveryPhrase
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Options.Applicative
    ( CommandFields, Mod, command, helper, info, progDesc, subparser )

import qualified Command.RecoveryPhrase.Generate as Generate


newtype Cmd
    = Generate Generate.Cmd
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "recovery-phrase" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "About recovery phrases."
  where
    parser = subparser $ mconcat
        [ Generate.mod Generate
        ]

run :: Cmd -> IO ()
run = \case
    Generate sub -> Generate.run sub
