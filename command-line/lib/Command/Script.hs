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
    , scriptParser
    ) where

import Cardano.Multisig
    ( Script (..), keyHashFromBytes, toScriptHash, validateScript )
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
            , indent 2 $ bold $ string "$ cat script.txt"
            , indent 2 $ bold $ string "$ all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f333]"
            , indent 2 $ bold $ string "$ cat script.txt \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" scripthash --base16 \\"
            , indent 2 $ string "a015ae61075e25c3d9250bdcbc35c6557272127927ecf2a2d716e29f"
            ])
  where
    parser = Cmd
        <$> encodingOpt [humanReadablePart|xpub|]

run :: Cmd -> IO ()
run Cmd{encoding} = do
    bytes <- B8.filter noNewline <$> B8.hGetContents stdin
    case readP_to_S scriptParser (B8.unpack bytes) of
         [(multisig,_rest)] ->
             if (validateScript multisig) then
                 hPutBytes stdout (toScriptHash multisig) encoding
             else
                 fail "The script is invalid."
         _ ->
             fail "Parsing of the script failed."

-- | The script embodies combination of signing keys that need to be met to make
-- it valid. We assume here that the script could
-- delivered from standard input. The examples below are self-explanatory:
--
-- 1. requiring signature
-- 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe
--
-- 2. 'any' for signature required
-- any [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]
--
-- 3. 'all' signatures required
-- all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]
--
-- 4. 'at_least' 1 signature required
-- at_least 1 [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]
--
-- 5. Nested script are supported
-- at_least 1 [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]]
--
-- Parser is insensitive to whitespaces.
--
-- @since 3.0.0
scriptParser :: ReadP Script
scriptParser =
    requireAllOfParser <++
    requireAnyOfParser <++
    requireAtLeastOfParser <++
    requireSignatureOfParser

requireSignatureOfParser :: ReadP Script
requireSignatureOfParser = do
    P.skipSpaces
    verKeyH <- P.munch1 (\c -> isDigit c || isLetter c)
    case length verKeyH of
        56 -> do
            let (Right bytes) = fromBase16 $ T.encodeUtf8 $ T.pack verKeyH
            let (Just keyHash) = keyHashFromBytes bytes
            return $ RequireSignatureOf keyHash
        len ->
            fail $ "Verification key hash should be 28 bytes, but received "
            <> show len <> " bytes."

requireAllOfParser :: ReadP Script
requireAllOfParser = do
    P.skipSpaces
    _identifier <- P.string "all"
    RequireAllOf <$> commonPart

requireAnyOfParser :: ReadP Script
requireAnyOfParser = do
    P.skipSpaces
    _identifier <- P.string "any"
    RequireAnyOf <$> commonPart

requireAtLeastOfParser :: ReadP Script
requireAtLeastOfParser = do
    P.skipSpaces
    _identifier <- P.string "at_least"
    RequireMOf <$> naturalParser <*> commonPart

naturalParser :: ReadP Word8
naturalParser = do
    P.skipSpaces
    fromInteger . read <$> P.munch1 isDigit

commonPart :: ReadP [Script]
commonPart = do
    P.skipSpaces
    _open <- P.string "["
    P.skipSpaces
    content <- P.sepBy1 scriptParser (P.string ",")
    P.skipSpaces
    _close <- P.string "]"
    P.skipSpaces
    return content
