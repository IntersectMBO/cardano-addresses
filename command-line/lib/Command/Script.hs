{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Script
    ( Cmd
    , mod
    , run

    -- * Internal
    , requireSignatureOfParser
    , requireAllOfParser
    , requireAnyOfParser
    , requireAtLeastOfParser
    ) where

import Cardano.Multisig
    ( MultisigScript (..), toScriptHash, verificationKeyHashFromBytes )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Codec.Binary.Encoding
    ( fromBase16 )
import Data.Char
    ( isDigit, isLetter )
import Data.Word
    ( Word8 )
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
    ( ReadP, readP_to_S, (<++) )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

scriptParser :: ReadP MultisigScript
scriptParser = do
    P.skipSpaces
    script <- multisigScriptParser
    P.skipSpaces
    return script

multisigScriptParser :: ReadP MultisigScript
multisigScriptParser =
    requireAllOfParser <++
    requireAnyOfParser <++
    requireAtLeastOfParser <++
    requireSignatureOfParser

requireSignatureOfParser :: ReadP MultisigScript
requireSignatureOfParser = do
    P.skipSpaces
    verKeyH <- P.munch1 (\c -> isDigit c || isLetter c)
    case length verKeyH of
        56 -> do
            let (Right bytes) = fromBase16 $ T.encodeUtf8 $ T.pack verKeyH
            return (RequireSignatureOf $ verificationKeyHashFromBytes bytes)
        len ->
            error $ "Verification key hash should be 28 bytes, but received "
            <> show len <> " bytes."

requireAllOfParser :: ReadP MultisigScript
requireAllOfParser = do
    P.skipSpaces
    _identifier <- P.string "all"
    RequireAllOf <$> commonPart

requireAnyOfParser :: ReadP MultisigScript
requireAnyOfParser = do
    P.skipSpaces
    _identifier <- P.string "any"
    RequireAnyOf <$> commonPart

requireAtLeastOfParser :: ReadP MultisigScript
requireAtLeastOfParser = do
    P.skipSpaces
    _identifier <- P.string "at_least"
    RequireMOf <$> naturalParser <*> commonPart

naturalParser :: ReadP Word8
naturalParser = do
    P.skipSpaces
    fromInteger . read <$> P.munch1 isDigit

commonPart :: ReadP [MultisigScript]
commonPart = do
    P.skipSpaces
    _open <- P.string "["
    P.skipSpaces
    content <- P.sepBy1 multisigScriptParser (P.string ",")
    P.skipSpaces
    _close <- P.string "]"
    P.skipSpaces
    return content
