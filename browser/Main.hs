{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Unified WASM executable for cardano-addresses browser operations.
-- Reads JSON with a "cmd" field from stdin, dispatches to the
-- appropriate handler, writes JSON to stdout.
module Main (main) where

import Prelude

import Cardano.Address
    ( unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , Index
    , XPrv
    , indexFromWord32
    , toXPub
    , xprvToBytes
    , xpubToBytes
    )
import Cardano.Address.Style.Shelley
    ( Role (..)
    )
import Cardano.Mnemonic
    ( MkSomeMnemonicError (..)
    , mkSomeMnemonic
    )
import Codec.Binary.Encoding
    ( AbstractEncoding (..)
    , detectEncoding
    , fromBase16
    , fromBase58
    , fromBech32
    )
import Data.Word
    ( Word32
    )
import System.Exit
    ( exitFailure
    )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

main :: IO ()
main = do
    raw <- BS.getContents
    case Json.eitherDecodeStrict' raw of
        Left err -> die' ("JSON parse error: " <> err)
        Right obj -> dispatch obj

dispatch :: Json.Object -> IO ()
dispatch obj = case lookupText "cmd" obj of
    Just "inspect" -> cmdInspect obj
    Just "derive" -> cmdDerive obj
    Just other -> die' ("Unknown command: " <> T.unpack other)
    Nothing -> die' "Missing \"cmd\" field"

------------------------------------------------------------------------
-- inspect
------------------------------------------------------------------------

cmdInspect :: Json.Object -> IO ()
cmdInspect obj = do
    addr <- requireText "address" obj
    let raw = T.encodeUtf8 addr
    let txt = T.unpack addr
    case detectEncoding txt of
        Nothing ->
            die' "Cannot detect address encoding"
        Just EBase16 ->
            case fromBase16 raw of
                Right bytes -> inspect bytes
                Left _ -> die' "Invalid hex encoding"
        Just (EBech32 _) ->
            case fromBech32 noMark raw of
                Right (_, bytes) -> inspect bytes
                Left e -> die' e
        Just EBase58 ->
            case fromBase58 raw of
                Right bytes -> inspect bytes
                Left e -> die' e
  where
    inspect bytes =
        case Shelley.eitherInspectAddress Nothing (unsafeMkAddress bytes) of
            Right json ->
                BL8.putStrLn (Json.encodePretty json)
            Left e -> die' (show e)

    noMark :: [Int] -> String -> String
    noMark _ s = s

------------------------------------------------------------------------
-- derive
------------------------------------------------------------------------

cmdDerive :: Json.Object -> IO ()
cmdDerive obj = do
    mnemonicText <- requireText "mnemonic" obj
    let pathText = lookupText "path" obj

    -- Parse mnemonic
    let wrds = T.words mnemonicText
    someMnemonic <- case mkSomeMnemonic @'[9, 12, 15, 18, 21, 24] wrds of
        Left (MkSomeMnemonicError e) -> die' e >> error "unreachable"
        Right mw -> pure mw

    -- Generate Shelley root key (empty passphrase)
    let rootK = Shelley.getKey
            (Shelley.genMasterKeyFromMnemonic someMnemonic mempty)

    case pathText of
        Nothing -> outputKeys
            [ ("root_xsk", xprvToBytes rootK)
            , ("root_xvk", xpubToBytes (toXPub rootK))
            ]
        Just path -> deriveAlongPath rootK path

deriveAlongPath :: XPrv -> T.Text -> IO ()
deriveAlongPath rootK path = do
    let segments = T.splitOn "/" path
    case segments of
        -- Full CIP-1852: purpose/coin_type/account/role/index
        [_purposeT, _coinTypeT, accountT, roleT, indexT] -> do
            accountIx <- parseHardenedIndex accountT
            role <- parseRole roleT
            addrIx <- parseSoftIndex indexT
            let rootShelley = Shelley.liftXPrv rootK
                acctK = Shelley.deriveAccountPrivateKey rootShelley accountIx
                addrK = Shelley.deriveAddressPrivateKey acctK role addrIx
                stakeK = Shelley.deriveDelegationPrivateKey acctK
            outputKeys
                [ ("root_xsk", xprvToBytes rootK)
                , ("root_xvk", xpubToBytes (toXPub rootK))
                , ("acct_xsk", xprvToBytes (Shelley.getKey acctK))
                , ("acct_xvk", xpubToBytes (toXPub (Shelley.getKey acctK)))
                , ("addr_xsk", xprvToBytes (Shelley.getKey addrK))
                , ("addr_xvk", xpubToBytes (toXPub (Shelley.getKey addrK)))
                , ("stake_xsk", xprvToBytes (Shelley.getKey stakeK))
                , ("stake_xvk", xpubToBytes (toXPub (Shelley.getKey stakeK)))
                ]

        -- Account only: purpose/coin_type/account
        [_purposeT, _coinTypeT, accountT] -> do
            accountIx <- parseHardenedIndex accountT
            let rootShelley = Shelley.liftXPrv rootK
                acctK = Shelley.deriveAccountPrivateKey rootShelley accountIx
            outputKeys
                [ ("root_xsk", xprvToBytes rootK)
                , ("root_xvk", xpubToBytes (toXPub rootK))
                , ("acct_xsk", xprvToBytes (Shelley.getKey acctK))
                , ("acct_xvk", xpubToBytes (toXPub (Shelley.getKey acctK)))
                ]

        _ -> die' ("Unsupported path format: " <> T.unpack path)

------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------

outputKeys :: [(T.Text, BS.ByteString)] -> IO ()
outputKeys pairs = BL8.putStrLn . Json.encodePretty . Json.Object $
    KM.fromList
        [ (Key.fromText k, Json.String (T.decodeUtf8 (Base16.encode v)))
        | (k, v) <- pairs
        ]

parseHardenedIndex
    :: T.Text
    -> IO (Index 'Hardened 'AccountK)
parseHardenedIndex t = do
    let cleaned = T.dropWhileEnd (== 'H') t
    n <- case reads (T.unpack cleaned) of
        [(v, "")] -> pure (v :: Word32)
        _ -> die' ("Invalid index: " <> T.unpack t) >> error "unreachable"
    case indexFromWord32 (0x80000000 + n) of
        Just ix -> pure ix
        Nothing ->
            die' ("Index out of range: " <> T.unpack t)
                >> error "unreachable"

parseSoftIndex
    :: T.Text
    -> IO (Index 'Soft 'PaymentK)
parseSoftIndex t = do
    n <- case reads (T.unpack t) of
        [(v, "")] -> pure (v :: Word32)
        _ -> die' ("Invalid index: " <> T.unpack t) >> error "unreachable"
    case indexFromWord32 n of
        Just ix -> pure ix
        Nothing ->
            die' ("Index out of range: " <> T.unpack t)
                >> error "unreachable"

parseRole :: T.Text -> IO Role
parseRole = \case
    "0" -> pure UTxOExternal
    "1" -> pure UTxOInternal
    "2" -> pure Stake
    "external" -> pure UTxOExternal
    "internal" -> pure UTxOInternal
    "stake" -> pure Stake
    other -> die' ("Unknown role: " <> T.unpack other)
                >> error "unreachable"

requireText :: T.Text -> Json.Object -> IO T.Text
requireText key obj = case KM.lookup (Key.fromText key) obj of
    Just (Json.String v) -> pure v
    _ -> die' ("Missing required field: " <> T.unpack key)
            >> error "unreachable"

lookupText :: T.Text -> Json.Object -> Maybe T.Text
lookupText key obj = case KM.lookup (Key.fromText key) obj of
    Just (Json.String v) -> Just v
    _ -> Nothing

die' :: String -> IO ()
die' msg = do
    B8.putStrLn $ "Error: " <> B8.pack msg
    exitFailure
