{-# LANGUAGE OverloadedStrings #-}

-- | Minimal WASM executable for address inspection.
-- Reads a bech32/base58/hex address from stdin, writes JSON to stdout.
module Main (main) where

import Prelude

import Cardano.Address
    ( unsafeMkAddress )
import Codec.Binary.Encoding
    ( AbstractEncoding (..)
    , detectEncoding
    , fromBase16
    , fromBase58
    , fromBech32
    )
import Control.Exception
    ( displayException )
import System.Exit
    ( exitFailure )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

main :: IO ()
main = do
    raw <- B8.filter (/= '\n') <$> BS.getContents
    let txt = T.unpack $ T.decodeUtf8 raw
    case detectEncoding txt of
        Nothing -> do
            B8.putStrLn "Error: cannot detect address encoding"
            exitFailure
        Just EBase16 ->
            case fromBase16 raw of
                Right bytes -> inspect bytes
                Left _ -> die' "invalid hex encoding"
        Just (EBech32 _) ->
            case fromBech32 noMark raw of
                Right (_, bytes) -> inspect bytes
                Left e -> die' e
        Just EBase58 ->
            case fromBase58 raw of
                Right bytes -> inspect bytes
                Left e -> die' e
        Just _ -> die' "unsupported encoding"
  where
    inspect bytes =
        case Shelley.eitherInspectAddress Nothing (unsafeMkAddress bytes) of
            Right json ->
                BL8.putStrLn (Json.encodePretty json)
            Left e -> die' (displayException e)

    noMark :: [Int] -> String -> String
    noMark _ s = s

    die' msg = do
        B8.putStrLn $ "Error: " <> B8.pack msg
        exitFailure
