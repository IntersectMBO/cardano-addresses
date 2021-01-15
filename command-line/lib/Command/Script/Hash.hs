{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Script.Hash
    ( Cmd
    , mod
    , run

    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Script
    ( KeyHash, Script (..), ScriptHash (..), toScriptHash )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, header, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import Options.Applicative.Script
    ( scriptArg )
import System.IO
    ( stdout )
import System.IO.Extra
    ( hPutBytes, progName )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5

newtype Cmd = Cmd
    { script :: Script KeyHash
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "hash" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a script hash"
        <> header "Create a script hash that can be used in stake or payment address."
        <> footerDoc (Just $ vsep
            [ string "The script is taken as argument."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string $ progName<>" script hash 'all "
            , indent 4 $ bold $ string "[ script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyreluzt36ms"
            , indent 4 $ bold $ string ", script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyrenxv223vj"
            , indent 4 $ bold $ string "]'"
            , indent 2 $ string "script15q26ucg8tcju8kf9p0wtcdwx24e8yyneylk09gkhzm3f70947hv"
            ])
  where
    parser = Cmd
        <$> scriptArg

run :: Cmd -> IO ()
run Cmd{script} = do
    let (ScriptHash bytes) = toScriptHash script
    hPutBytes stdout bytes (EBech32 CIP5.script)
