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
    ( Address
    , AddressDiscrimination (..)
    , NetworkTag (..)
    , base58
    , bech32
    , unsafeMkAddress
    )
import Cardano.Address.Crypto.Wallet
    ( unXSignature
    , xsignature
    )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , Index
    , XPrv
    , XPub
    , indexFromWord32
    , sign
    , wholeDomainIndex
    , toXPub
    , verify
    , xprvFromBytes
    , xprvToBytes
    , xpubFromBytes
    , xpubToBytes
    )
import Cardano.Address.Style.Shelley
    ( Credential (..)
    , Role (..)
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

import qualified Cardano.Address as CA
import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.ByteArray.Encoding
    ( Base (..)
    , convertToBase
    )
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
    Just "make-address" -> cmdMakeAddress obj
    Just "sign" -> cmdSign obj
    Just "verify" -> cmdVerify obj
    Just "bootstrap-address" -> cmdBootstrapAddress obj
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
-- make-address
------------------------------------------------------------------------

cmdMakeAddress :: Json.Object -> IO ()
cmdMakeAddress obj = do
    addrType <- requireText "type" obj
    networkText <- requireText "network" obj
    network <- parseNetwork networkText

    case addrType of
        "enterprise" -> do
            paymentKeyHex <- requireText "payment_key" obj
            paymentXPub <- parseXPub paymentKeyHex
            let addr = Shelley.paymentAddress network
                    (PaymentFromExtendedKey (Shelley.liftXPub paymentXPub))
            outputAddress addr

        "base" -> do
            paymentKeyHex <- requireText "payment_key" obj
            stakeKeyHex <- requireText "stake_key" obj
            paymentXPub <- parseXPub paymentKeyHex
            stakeXPub <- parseXPub stakeKeyHex
            let addr = Shelley.delegationAddress network
                    (PaymentFromExtendedKey (Shelley.liftXPub paymentXPub))
                    (DelegationFromExtendedKey (Shelley.liftXPub stakeXPub))
            outputAddress addr

        "reward" -> do
            stakeKeyHex <- requireText "stake_key" obj
            stakeXPub <- parseXPub stakeKeyHex
            case Shelley.stakeAddress network
                    (DelegationFromExtendedKey (Shelley.liftXPub stakeXPub)) of
                Right addr -> outputAddress addr
                Left e -> die' (show e)

        other -> die' ("Unknown address type: " <> T.unpack other)

outputAddress :: Address -> IO ()
outputAddress addr = BL8.putStrLn . Json.encodePretty . Json.Object $
    KM.fromList
        [ ("address_bech32", Json.String (bech32 addr))
        ]

parseNetwork
    :: T.Text
    -> IO (CA.NetworkDiscriminant Shelley.Shelley)
parseNetwork = \case
    "mainnet" -> pure Shelley.shelleyMainnet
    "testnet" -> pure Shelley.shelleyTestnet
    other -> case Shelley.mkNetworkDiscriminant (read (T.unpack other)) of
        Right n -> pure n
        Left _ -> die' ("Invalid network: " <> T.unpack other)
            >> error "unreachable"

parseXPub :: T.Text -> IO XPub
parseXPub hex = do
    let bs = decodeHex hex
    case xpubFromBytes bs of
        Just xpub -> pure xpub
        Nothing -> die' "Invalid xpub bytes (expected 64 bytes)"
            >> error "unreachable"

------------------------------------------------------------------------
-- sign / verify
------------------------------------------------------------------------

cmdSign :: Json.Object -> IO ()
cmdSign obj = do
    keyHex <- requireText "key" obj
    messageHex <- requireText "message" obj
    let keyBytes = decodeHex keyHex
        msgBytes = decodeHex messageHex
    xprv <- case xprvFromBytes keyBytes of
        Just k -> pure k
        Nothing -> die' "Invalid signing key (expected 96 bytes xprv)"
            >> error "unreachable"
    let sig = sign xprv msgBytes
        vk = toXPub xprv
    outputKeys
        [ ("signature", unXSignature sig)
        , ("verification_key", xpubToBytes vk)
        ]

cmdVerify :: Json.Object -> IO ()
cmdVerify obj = do
    keyHex <- requireText "key" obj
    messageHex <- requireText "message" obj
    signatureHex <- requireText "signature" obj
    let keyBytes = decodeHex keyHex
        msgBytes = decodeHex messageHex
        sigBytes = decodeHex signatureHex
    xpub <- case xpubFromBytes keyBytes of
        Just k -> pure k
        Nothing -> die' "Invalid verification key (expected 64 bytes xpub)"
            >> error "unreachable"
    sig <- case xsignature sigBytes of
        Right s -> pure s
        Left e -> die' ("Invalid signature: " <> e) >> error "unreachable"
    let result = verify xpub msgBytes sig
    BL8.putStrLn . Json.encodePretty . Json.Object $
        KM.fromList [("valid", Json.Bool result)]

------------------------------------------------------------------------
-- bootstrap-address (Byron / Icarus)
------------------------------------------------------------------------

cmdBootstrapAddress :: Json.Object -> IO ()
cmdBootstrapAddress obj = do
    style <- requireText "style" obj
    protocolMagic <- requireInt "protocol_magic" obj
    case style of
        "icarus-from-mnemonic" -> do
            mnemonicText <- requireText "mnemonic" obj
            accountIndex <- requireInt "account_index" obj
            roleIndex <- requireInt "role" obj
            addressIndex <- requireInt "address_index" obj
            bootstrapIcarusFromMnemonic protocolMagic mnemonicText accountIndex roleIndex addressIndex
        "byron-from-mnemonic" -> do
            mnemonicText <- requireText "mnemonic" obj
            accountIndex <- requireInt "account_index" obj
            addressIndex <- requireInt "address_index" obj
            bootstrapByronFromMnemonic protocolMagic mnemonicText accountIndex addressIndex
        "icarus" -> do
            xpubHex <- requireText "xpub" obj
            bootstrapIcarusFromXPub protocolMagic xpubHex
        "byron" -> do
            xpubHex <- requireText "xpub" obj
            rootXPubHex <- requireText "root_xpub" obj
            derivationPath <- requireText "derivation_path" obj
            bootstrapByronFromXPub protocolMagic xpubHex rootXPubHex derivationPath
        other -> die' ("Unknown bootstrap style: " <> T.unpack other)

bootstrapIcarusFromMnemonic :: Int -> T.Text -> Int -> Int -> Int -> IO ()
bootstrapIcarusFromMnemonic protocolMagic mnemonicText accountIndex roleIndex addressIndex = do
    let wrds = T.words mnemonicText
    someMnemonic <- case mkSomeMnemonic @'[9, 12, 15, 18, 21, 24] wrds of
        Left (MkSomeMnemonicError e) -> die' e >> error "unreachable"
        Right mw -> pure mw
    let rootK = Icarus.genMasterKeyFromMnemonic someMnemonic mempty
    acctIx <- parseHardenedIndex' accountIndex
    addrIx <- parseSoftIndex' addressIndex
    let icarusRole = if roleIndex == 0 then Icarus.UTxOExternal else Icarus.UTxOInternal
        acctK = Icarus.deriveAccountPrivateKey rootK acctIx
        addrK = Icarus.deriveAddressPrivateKey acctK icarusRole addrIx
        addrXPub = toXPub <$> addrK
        networkDisc = icarusNetworkDiscriminant protocolMagic
        addr = Icarus.paymentAddress networkDisc addrXPub
    outputBase58Address addr

bootstrapByronFromMnemonic :: Int -> T.Text -> Int -> Int -> IO ()
bootstrapByronFromMnemonic protocolMagic mnemonicText accountIndex addressIndex = do
    let wrds = T.words mnemonicText
    someMnemonic <- case mkSomeMnemonic @'[9, 12, 15, 18, 21, 24] wrds of
        Left (MkSomeMnemonicError e) -> die' e >> error "unreachable"
        Right mw -> pure mw
    let rootK = Byron.genMasterKeyFromMnemonic someMnemonic
        acctIx = wholeDomainIndex (0x80000000 + fromIntegral accountIndex)
        addrIx = wholeDomainIndex (fromIntegral addressIndex)
        acctK = Byron.deriveAccountPrivateKey rootK acctIx
        addrK = Byron.deriveAddressPrivateKey acctK addrIx
        addrXPub = toXPub <$> addrK
        networkDisc = byronNetworkDiscriminant protocolMagic
        addr = Byron.paymentAddress networkDisc addrXPub
    outputBase58Address addr

bootstrapIcarusFromXPub :: Int -> T.Text -> IO ()
bootstrapIcarusFromXPub protocolMagic xpubHex = do
    xpub <- parseXPub xpubHex
    let networkDisc = icarusNetworkDiscriminant protocolMagic
        addr = Icarus.paymentAddress networkDisc (Icarus.liftXPub xpub)
    outputBase58Address addr

bootstrapByronFromXPub :: Int -> T.Text -> T.Text -> T.Text -> IO ()
bootstrapByronFromXPub protocolMagic xpubHex rootXPubHex derivationPath = do
    addrXPub <- parseXPub xpubHex
    rootXPub <- parseXPub rootXPubHex
    let segments = T.splitOn "/" derivationPath
    case segments of
        [acctT, addrT] -> do
            let acctN = read (T.unpack (T.dropWhileEnd (== 'H') acctT)) :: Word32
                addrN = read (T.unpack addrT) :: Word32
                acctIx = wholeDomainIndex (0x80000000 + acctN)
                addrIx = wholeDomainIndex addrN
                byronKey = Byron.liftXPub rootXPub (acctIx, addrIx) addrXPub
                networkDisc = byronNetworkDiscriminant protocolMagic
                addr = Byron.paymentAddress networkDisc byronKey
            outputBase58Address addr
        _ -> die' ("Byron derivation path must have 2 segments: " <> T.unpack derivationPath)

outputBase58Address :: Address -> IO ()
outputBase58Address addr = BL8.putStrLn . Json.encodePretty . Json.Object $
    KM.fromList
        [ ("address_base58", Json.String (base58 addr))
        ]

icarusNetworkDiscriminant :: Int -> CA.NetworkDiscriminant Icarus.Icarus
icarusNetworkDiscriminant pm = case pm of
    764824073 -> Icarus.icarusMainnet
    633343913 -> Icarus.icarusStaging
    1097911063 -> Icarus.icarusTestnet
    2 -> Icarus.icarusPreview
    1 -> Icarus.icarusPreprod
    magic -> (CA.RequiresNetworkTag, CA.NetworkTag (fromIntegral magic))

byronNetworkDiscriminant :: Int -> CA.NetworkDiscriminant Byron.Byron
byronNetworkDiscriminant pm = case pm of
    764824073 -> Byron.byronMainnet
    633343913 -> Byron.byronStaging
    1097911063 -> Byron.byronTestnet
    2 -> Byron.byronPreview
    1 -> Byron.byronPreprod
    magic -> (CA.RequiresNetworkTag, CA.NetworkTag (fromIntegral magic))

parseHardenedIndex' :: Int -> IO (Index 'Hardened 'AccountK)
parseHardenedIndex' n = case indexFromWord32 (0x80000000 + fromIntegral n) of
    Just ix -> pure ix
    Nothing -> die' ("Index out of range: " <> show n) >> error "unreachable"

parseSoftIndex' :: Int -> IO (Index 'Soft 'PaymentK)
parseSoftIndex' n = case indexFromWord32 (fromIntegral n) of
    Just ix -> pure ix
    Nothing -> die' ("Index out of range: " <> show n) >> error "unreachable"

------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------

decodeHex :: T.Text -> BS.ByteString
decodeHex = decodeHexStr . T.unpack . T.strip

decodeHexStr :: String -> BS.ByteString
decodeHexStr str = BS.pack (go str)
  where
    go [] = []
    go [_] = error ("Odd-length hex: " <> str)
    go (a:b:rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : go rest

digitToInt :: Char -> Int
digitToInt c
    | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
    | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
    | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
    | otherwise = error ("Invalid hex character: " <> [c])

toHex :: BS.ByteString -> T.Text
toHex = T.decodeUtf8 . convertToBase Base16

outputKeys :: [(T.Text, BS.ByteString)] -> IO ()
outputKeys pairs = BL8.putStrLn . Json.encodePretty . Json.Object $
    KM.fromList
        [ (Key.fromText k, Json.String (toHex v))
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

requireInt :: T.Text -> Json.Object -> IO Int
requireInt key obj = case KM.lookup (Key.fromText key) obj of
    Just (Json.Number n) -> pure (round n)
    _ -> die' ("Missing required integer field: " <> T.unpack key)
            >> error "unreachable"

die' :: String -> IO ()
die' msg = do
    B8.putStrLn $ "Error: " <> B8.pack msg
    exitFailure
