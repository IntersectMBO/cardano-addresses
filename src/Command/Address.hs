{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Address
    ( Cmd
    , mod
    , run
    ) where

import Options.Applicative
    ( CommandFields, Mod, command, helper, info, progDesc, subparser )
import Prelude hiding
    ( mod )

import qualified Command.Address.Bootstrap as Bootstrap
import qualified Command.Address.Payment as Payment

--
-- address
--

data Cmd
    = Bootstrap Bootstrap.Cmd
    | Payment Payment.Cmd
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "address" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "About addresses."
  where
    parser = subparser $ mconcat
        [ Bootstrap.mod Bootstrap
        , Payment.mod Payment
        ]

run :: Cmd -> IO ()
run = \case
    Bootstrap sub -> Bootstrap.run sub
    Payment sub -> Payment.run sub
