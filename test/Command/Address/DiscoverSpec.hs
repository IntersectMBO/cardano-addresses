{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Command.Address.DiscoverSpec
    ( spec
    ) where

import Prelude

import Data.Word
    ( Word32 )
import Test.Hspec
    ( Spec, SpecWith, describe, expectationFailure, it, shouldBe, shouldContain, shouldNotContain )
import Test.Utils
    ( SchemaRef, cli, describeCmd, validateJSON )

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BL8

spec :: Spec
spec = describeCmd [ "address", "discover" ] $ do
    describe "golden TSV output matches library fixtures" $ do
        specDiscoverTSV "mainnet base gap 1" "mainnet" "base" 1
            [ "payment_external\t0\taddr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqfjkjv7"
            , "payment_internal\t0\taddr1qx4kjk0as0x7ptt54l6cnfyzejqg22cku0qhqx6al4g2xepjcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqhpmech"
            , "stake_credential\t0\tstake_vkh1xtrj35uxrctye2egew8sqezgzwwg796ql7uw02572gedc78jpfa"
            , "drep_credential\t0\tdrep1y2jmg4g450lced7q9n34rq6d5vjwkm0ugx6h0894u6ur92s9txn3a"
            , "committee_cold_credential\t0\tcc_cold1ztl0h9vka4ns45kfj7xh3ljwkd46yn9m5znzlfxd6rpdeagw6p59q"
            , "committee_hot_credential\t0\tcc_hot1qtmd98q0w9jdxasse0m8kyn2nya7kf9qwmgx20cl5p543rcdtr4dz"
            ]

        specDiscoverTSV "mainnet enterprise gap 1" "mainnet" "enterprise" 1
            [ "payment_external\t0\taddr1vx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers66hrl8"
            , "payment_internal\t0\taddr1vx4kjk0as0x7ptt54l6cnfyzejqg22cku0qhqx6al4g2xeqryym03"
            ]

        specDiscoverTSV "testnet base gap 1" "testnet" "base" 1
            [ "payment_external\t0\taddr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp"
            , "payment_internal\t0\taddr_test1qz4kjk0as0x7ptt54l6cnfyzejqg22cku0qhqx6al4g2xepjcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq5hxe5g"
            ]

        specDiscoverTSV "testnet enterprise gap 1" "testnet" "enterprise" 1
            [ "payment_external\t0\taddr_test1vz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspjrlsz"
            , "payment_internal\t0\taddr_test1vz4kjk0as0x7ptt54l6cnfyzejqg22cku0qhqx6al4g2xeqcvs8q5"
            ]

    describe "golden JSON output" $ do
        specDiscoverJSON "mainnet base gap 1" "mainnet" "base" 1
            [ "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqfjkjv7"
            , "addr1qx4kjk0as0x7ptt54l6cnfyzejqg22cku0qhqx6al4g2xepjcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqhpmech"
            , "stake_vkh1xtrj35uxrctye2egew8sqezgzwwg796ql7uw02572gedc78jpfa"
            , "drep1y2jmg4g450lced7q9n34rq6d5vjwkm0ugx6h0894u6ur92s9txn3a"
            ]

    describe "root key input derives default account 0" $ do
        it "produces same addresses as acct_xvk" $ do
            outRoot <- cliFromRootKey [ "address", "discover", "--gap", "1", "--type", "base", "--network-tag", "mainnet" ]
            outAcct <- cliFromAccountKey [ "address", "discover", "--gap", "1", "--type", "base", "--network-tag", "mainnet" ]
            outRoot `shouldBe` outAcct

    describe "--from offset" $ do
        it "starts payment indices at the given offset" $ do
            out <- cliFromAccountKey
                [ "address", "discover", "--gap", "2", "--from", "5", "--type", "base", "--network-tag", "mainnet" ]
            out `shouldContain` "payment_external\t5\t"
            out `shouldNotContain` "payment_external\t0\t"
            out `shouldNotContain` "payment_external\t1\t"

    describe "invalid inputs" $ do
        it "rejects invalid network tag" $ do
            (out, err) <- cliFromAccountKeyBoth
                [ "address", "discover", "--gap", "1", "--type", "base", "--network-tag", "16" ]
            out `shouldBe` ""
            err `shouldContain` "Network tag 16 is invalid"

        it "rejects unknown address type" $ do
            (out, err) <- cliFromAccountKeyBoth
                [ "address", "discover", "--gap", "1", "--type", "unknown", "--network-tag", "mainnet" ]
            out `shouldBe` ""
            err `shouldContain` "Unknown address type"

        it "rejects non-bech32 key input" $ do
            (out, err) <- cli [ "address", "discover", "--gap", "1", "--type", "base", "--network-tag", "mainnet" ] "not-a-key" :: IO (String, String)
            out `shouldBe` ""
            err `shouldContain` "Bech32"

--
-- Golden helpers
--

specDiscoverTSV :: String -> String -> String -> Word32 -> [String] -> SpecWith ()
specDiscoverTSV name networkTag typ gap mustContain = it ("golden tsv " <> name) $ do
    out <- cliFromAccountKey
        [ "address", "discover"
        , "--gap", show gap
        , "--type", typ
        , "--network-tag", networkTag
        ]
    mapM_ (\needle -> out `shouldContain` needle) mustContain

specDiscoverJSON :: String -> String -> String -> Word32 -> [String] -> SpecWith ()
specDiscoverJSON name networkTag typ gap mustContain = it ("golden json " <> name) $ do
    out <- cliFromAccountKey
        [ "address", "discover"
        , "--gap", show gap
        , "--type", typ
        , "--network-tag", networkTag
        , "--json"
        ]
    case Json.eitherDecode (BL8.pack out) of
        Left e -> expectationFailure $ "malformed JSON: " <> show e
        Right json -> validateJSON schema json >>= \case
            [] -> mapM_ (\needle -> out `shouldContain` needle) mustContain
            es -> expectationFailure $ "invalid JSON: " <> unlines es
  where
    schema :: SchemaRef
    schema = "./schemas/address-discover.json"

--
-- Key helpers
--

testPhrase :: String
testPhrase = unwords
    [ "test", "walk", "nut", "penalty", "hip", "pave"
    , "soap", "entry", "language", "right", "filter", "choice"
    ]

cliFromAccountKey :: [String] -> IO String
cliFromAccountKey args =
    cli [ "key", "from-recovery-phrase", "shelley" ] testPhrase
        >>= cli [ "key", "child", "1852H/1815H/0H" ]
        >>= cli [ "key", "public", "--with-chain-code" ]
        >>= cli args

cliFromRootKey :: [String] -> IO String
cliFromRootKey args =
    cli [ "key", "from-recovery-phrase", "shelley" ] testPhrase
        >>= cli args

cliFromAccountKeyBoth :: [String] -> IO (String, String)
cliFromAccountKeyBoth args = do
    (out1, _) <- cli [ "key", "from-recovery-phrase", "shelley" ] testPhrase :: IO (String, String)
    (out2, _) <- cli [ "key", "child", "1852H/1815H/0H" ] out1 :: IO (String, String)
    (out3, _) <- cli [ "key", "public", "--with-chain-code" ] out2 :: IO (String, String)
    cli args out3
