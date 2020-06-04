{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Address
    ( Cmd
    , mod
    , run
    ) where

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
import Prelude hiding
    ( mod )

import qualified Command.Address.Bootstrap as Bootstrap
import qualified Command.Address.Delegation as Delegation
import qualified Command.Address.Inspect as Inspect
import qualified Command.Address.Payment as Payment


data Cmd
    = Bootstrap Bootstrap.Cmd
    | Payment Payment.Cmd
    | Delegation Delegation.Cmd
    | Inspect Inspect.Cmd
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "address" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "About addresses"
        <> footerDoc (Just $ string $ mconcat
            [ "Integrating with Byron?\n  ↳ Look at 'bootstrap'."
            , "\n\n"
            , "Integrating with Shelley?\n  ↳ Look at 'payment' & 'delegation'."
            ])
  where
    parser = subparser $ mconcat
        [ Bootstrap.mod Bootstrap
        , Payment.mod Payment
        , Delegation.mod Delegation
        , Inspect.mod Inspect
        ]

run :: Cmd -> IO ()
run = \case
    Bootstrap sub -> Bootstrap.run sub
    Payment sub -> Payment.run sub
    Delegation sub -> Delegation.run sub
    Inspect sub -> Inspect.run sub
