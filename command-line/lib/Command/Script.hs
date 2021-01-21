{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Script
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

import qualified Command.Script.Hash as Hash
import qualified Command.Script.Validation as Validation

data Cmd
    = Hash Hash.Cmd
    | Validation Validation.Cmd
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "script" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "About scripts"
        <> footerDoc (Just $ vsep
            [ string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Shelley > root.xprv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.xprv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1852H/1815H/0H/3/0 > signingKey1.xprv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat signingKey1.xprv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public | " <>progName<>" key hash > verKey1.hash"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.xprv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1852H/1815H/0H/3/1 > signingKey2.xprv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat signingKey2.xprv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public | " <>progName<>" key hash > verKey2.hash"
            , indent 2 $ string ""
            , indent 2 $ bold $ string $ "$ "<>progName<>" script hash \"all [$(cat verKey1.hash),$(cat verKey2.hash)]\""
            ])
  where
    parser = subparser $ mconcat
        [ Hash.mod Hash
        , Validation.mod Validation
        ]

run :: Cmd -> IO ()
run = \case
    Hash sub -> Hash.run sub
    Validation sub -> Validation.run sub
