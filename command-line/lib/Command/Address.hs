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
    ( annotate, bold, hsep, pretty, vsep )
import Prelude hiding
    ( mod )

import qualified Command.Address.Bootstrap as Bootstrap
import qualified Command.Address.Delegation as Delegation
import qualified Command.Address.Inspect as Inspect
import qualified Command.Address.Payment as Payment
import qualified Command.Address.Pointer as Pointer
import qualified Command.Address.Reward as Reward


data Cmd
    = Bootstrap Bootstrap.Cmd
    | Payment Payment.Cmd
    | Reward Reward.Cmd
    | Delegation Delegation.Cmd
    | Pointer Pointer.Cmd
    | Inspect Inspect.Cmd
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "address" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "About addresses"
        <> footerDoc (Just $ vsep
            [ pretty "Integrating with Byron?"
            , hsep [ pretty "  ↳ Look at", annotate bold $ pretty "'bootstrap'", pretty "." ]
            , pretty ""
            , pretty "Integrating with Shelley?"
            , hsep [ pretty "  ↳ Look at", annotate bold $ pretty "'payment'", pretty "&", annotate bold $ pretty "'delegation'", pretty "." ]
            ])
  where
    parser = subparser $ mconcat
        [ Bootstrap.mod Bootstrap
        , Payment.mod Payment
        , Reward.mod Reward
        , Delegation.mod Delegation
        , Pointer.mod Pointer
        , Inspect.mod Inspect
        ]

run :: Cmd -> IO ()
run = \case
    Bootstrap sub -> Bootstrap.run sub
    Payment sub -> Payment.run sub
    Reward sub -> Reward.run sub
    Delegation sub -> Delegation.run sub
    Pointer sub -> Pointer.run sub
    Inspect sub -> Inspect.run sub
