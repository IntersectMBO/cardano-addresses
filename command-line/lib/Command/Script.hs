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

newtype Cmd
    = Hash Hash.Cmd
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "script" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "About script"
        <> footerDoc (Just $ vsep
            [ string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Shelley > root.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1852H/1815H/0H/3/0 > signingKey1.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat signingKey1.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public | " <>progName<>" key hash --base16 > verKeyHash1"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1852H/1815H/0H/3/1 > signingKey2.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat signingKey2.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public | " <>progName<>" key hash --base16 > verKeyHash2"
            , indent 2 $ string ""
            , indent 2 $ bold $ string $ "$ "<>progName<>" script hash --base16 \"all [$(cat verKeyHash1),$(cat verKeyHash2)]\""
            ])
  where
    parser = subparser $ mconcat
        [ Hash.mod Hash
        ]

run :: Cmd -> IO ()
run = \case
    Hash sub -> Hash.run sub
