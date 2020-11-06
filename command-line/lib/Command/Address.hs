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
    ( bold, hsep, string, vsep )
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
            [ string "Integrating with Byron?"
            , hsep [ string "  ↳ Look at", bold $ string "'bootstrap'", string "." ]
            , string ""
            , string "Integrating with Shelley?"
            , hsep [ string "  ↳ Look at", bold $ string "'payment'", string "&", bold $ string "'delegation'", string "." ]
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
