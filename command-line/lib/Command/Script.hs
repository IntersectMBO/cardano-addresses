{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Script
    ( Cmd
    , mod
    , run
    ) where

import Cardano.Multisig
    ( MultisigScript (..), toScriptHash, verificationKeyHashFromBytes )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, header, helper, info, progDesc )
import Options.Applicative.Encoding
    ( Encoding, encodingOpt )
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import Prelude hiding
    ( mod )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hPutBytes, noNewline, progName )
import Text.ParserCombinators.ReadP
    ( readP_to_S )

import qualified Data.ByteString.Char8 as B8
import qualified Text.ParserCombinators.ReadP as P

newtype Cmd = Cmd
    { encoding :: Encoding
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "scripthash" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a script hash"
        <> header "Create a script hash \
            \that can be used in stake or payment address."
        <> footerDoc (Just $ vsep
            [ string "The script is read from stdin."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string "$ cat script.txt \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" scripthash \\"
            , indent 2 $ string "script_test1uzp7swuxjx7wmpkkvat8kpgrmjl8ze0dj9lytn25qv2tm4g6n5c35"
            ])
  where
    parser = Cmd
        <$> encodingOpt [humanReadablePart|xpub|]

run :: Cmd -> IO ()
run Cmd{encoding} = do
    bytes <- B8.filter noNewline <$> B8.hGetContents stdin
    case readP_to_S scriptParser (B8.unpack bytes) of
         [(multisig,_rest)] ->
             hPutBytes stdout (toScriptHash multisig) encoding
         _ ->
             error "parsing the script failed"

scriptParser :: P.ReadP MultisigScript
scriptParser = do
    P.skipSpaces
    script <- multisigScriptParser
    P.skipSpaces
    return script

multisigScriptParser :: P.ReadP MultisigScript
multisigScriptParser =
    requireSignatureOfParser

requireSignatureOfParser :: P.ReadP MultisigScript
requireSignatureOfParser = do
    verKeyH <- P.munch1 (\c -> c /= ' ' && c /= ',' && c /= ']')
    case length verKeyH of
        56 ->
            return (RequireSignatureOf $ verificationKeyHashFromBytes (B8.pack verKeyH))
        _ ->
            P.pfail
