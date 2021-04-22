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
import qualified Command.Script.Preimage as Preimage
import qualified Command.Script.Validation as Validation

data Cmd
    = Hash Hash.Cmd
    | Validation Validation.Cmd
    | Preimage Preimage.Cmd
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "script" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "About scripts"
        <> footerDoc (Just $ vsep
            [ string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Shared > root_shared.xsk"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root_shared.xsk \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1854H/1815H/0H/0/0 > signingKey1.xsk"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat signingKey1.xsk \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public --without-chain-code \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key hash > verKey1.vkh"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root_shared.xsk \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1854H/1815H/0H/0/1 > signingKey2.xsk"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat signingKey2.xsk \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public --without-chain-code \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key hash > verKey2.vkh"
            , indent 2 $ string ""
            , indent 2 $ bold $ string $ "$ "<>progName<>" script hash \"all [$(cat verKey1.vkh),$(cat verKey2.vkh)]\""
            , indent 2 $ string ""
            , indent 2 $ bold $ string $ "$ "<>progName<>" script preimage \"all [addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq, active_from 100, active_until 150]\""
            ])
  where
    parser = subparser $ mconcat
        [ Hash.mod Hash
        , Validation.mod Validation
        , Preimage.mod Preimage
        ]

run :: Cmd -> IO ()
run = \case
    Hash sub -> Hash.run sub
    Validation sub -> Validation.run sub
    Preimage sub -> Preimage.run sub
