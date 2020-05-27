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

import qualified Command.RecoveryPhrase as Create
--
-- address
--

-- cardano-address address init --network-tag 0 \
--  | cardano-address address add-key <<< "public key"
--  | cardano-address address add-pointer 1 2 3

newtype Cmd
    = Create Create.Cmd
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "address" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "About addresses."
  where
    parser = subparser $ mconcat
        [ Create.mod Create
        ]

run :: Cmd -> IO ()
run = \case
    Create sub -> Create.run sub
