{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Script.Hash
    ( Cmd
    , mod
    , run

    ) where

import Cardano.Script
    ( Script (..)
    , ScriptError (..)
    , ScriptHash (..)
    , toScriptHash
    , validateScript
    )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, header, helper, info, progDesc )
import Options.Applicative.Encoding
    ( Encoding, encodingOpt )
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import Options.Applicative.Script
    ( scriptArg )
import Prelude hiding
    ( mod )
import System.Exit
    ( die )
import System.IO
    ( stdout )
import System.IO.Extra
    ( hPutBytes, progName )

data Cmd = Cmd
    { encoding :: Encoding
    , script :: Script
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "hash" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a script hash"
        <> header "Create a script hash \
            \that can be used in stake or payment address."
        <> footerDoc (Just $ vsep
            [ string "The script is taken as argument."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string "$ cat script.txt"
            , indent 2 $ bold $ string "$ all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f333]"
            , indent 2 $ bold $ string "$ cat script.txt \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" script hash --base16 $(cat script.txt)\\"
            , indent 2 $ string "a015ae61075e25c3d9250bdcbc35c6557272127927ecf2a2d716e29f"
            ])
  where
    parser = Cmd
        <$> encodingOpt [humanReadablePart|script_hash|]
        <*> scriptArg

run :: Cmd -> IO ()
run Cmd{encoding, script} =
    case (validateScript script) of
        Left err ->
            die $ show $ InvalidScript err
        Right () -> do
            let (ScriptHash bytes) = toScriptHash script
            hPutBytes stdout bytes encoding
