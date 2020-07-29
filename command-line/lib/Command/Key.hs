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
    ( bold, indent, string, vsep )
import System.IO.Extra
    ( progName )

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
        <> footerDoc (Just $ vsep
            [ string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Shelley > root.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1852H/1815H/0H \\"
            , indent 4 $ bold $ string "| tee acct.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public > acct.pub"
            , indent 2 $ string ""
            , indent 2 $ bold $ string $ "$ "<>progName<>" key inspect <<< $(cat acct.prv)"
            , indent 2 $ string "{"
            , indent 2 $ string "    \"key_type\": \"private\","
            , indent 2 $ string "    \"chain_code\": \"67bef6f80df02c7452e20e76ffb4bb57cae8aac2adf042b21a6b19e4f7b1f511\","
            , indent 2 $ string "    \"extended_key\": \"90ead3efad7aacac242705ede323665387f49ed847bed025eb333708ccf6aa54403482a867daeb18f38c57d6cddd7e6fd6aed4a3209f7425a3d1c5d9987a9c5f\""
            , indent 2 $ string "}"
            , indent 2 $ string ""
            , indent 2 $ bold $ string $ "$ "<>progName<>" key inspect <<< $(cat acct.pub)"
            , indent 2 $ string "{"
            , indent 2 $ string "    \"key_type\": \"public\","
            , indent 2 $ string "    \"chain_code\": \"67bef6f80df02c7452e20e76ffb4bb57cae8aac2adf042b21a6b19e4f7b1f511\","
            , indent 2 $ string "    \"extended_key\": \"d306350ee88f51fb710252e27f0c40006c58e994761b383e02d400e2be59b3cc\""
            , indent 2 $ string "}"
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
