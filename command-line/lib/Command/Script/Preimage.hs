{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Script.Preimage
    ( Cmd
    , mod
    , run

    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Script
    ( KeyHash, Script (..), serializeScript )
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

newtype Cmd = Cmd
    { script :: Script KeyHash
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "preimage" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a script preimage"
        <> header "Create a script preimage that is CBOR-encoded script."
        <> footerDoc (Just $ vsep
            [ string "The script is taken as argument."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string $ progName<>" script preimage 'all "
            , indent 4 $ bold $ string "[ script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyreluzt36ms"
            , indent 4 $ bold $ string ", script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyrenxv223vj"
            , indent 4 $ bold $ string "]'"
            , indent 2 $ string "008201828200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe8200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f333"
            , string ""
            , indent 2 $ bold $ string $ progName<>" script preimage 'all "
            , indent 4 $ bold $ string "[ script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyreluzt36ms"
            , indent 4 $ bold $ string ", active_from 100, active_until 150"
            , indent 4 $ bold $ string "]'"
            , indent 2 $ string "008201838200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe8204186482051896"
            ])
  where
    parser = Cmd
        <$> scriptArg

run :: Cmd -> IO ()
run Cmd{script} = do
    hPutBytes stdout (serializeScript script) EBase16
