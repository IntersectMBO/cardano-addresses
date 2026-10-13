module Command.Script.InspectSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "script", "inspect" ] $ do
    specInspect scriptHashGeneric
        [ "\"role\": \"unknown\""
        , "\"script_hash\": \"" <> scriptHashGenericHex <> "\""
        , "\"credential\": \"" <> scriptHashGeneric <> "\""
        ]

    specInspect drepCIP0129
        [ "\"role\": \"representative\""
        , "\"script_hash\": \"" <> drepHex <> "\""
        , "\"credential\": \"" <> drepCIP0129 <> "\""
        , "\"legacy_credential\": \"" <> drepCIP0105 <> "\""
        ]

    specInspect drepCIP0105
        [ "\"role\": \"representative\""
        , "\"script_hash\": \"" <> drepHex <> "\""
        , "\"credential\": \"" <> drepCIP0129 <> "\""
        , "\"legacy_credential\": \"" <> drepCIP0105 <> "\""
        ]

    specInspect ccColdCIP0129
        [ "\"role\": \"committee_cold\""
        , "\"script_hash\": \"" <> ccColdHex <> "\""
        , "\"credential\": \"" <> ccColdCIP0129 <> "\""
        , "\"legacy_credential\": \"" <> ccColdCIP0105 <> "\""
        ]

    specInspect ccHotCIP0129
        [ "\"role\": \"committee_hot\""
        , "\"script_hash\": \"" <> ccHotHex <> "\""
        , "\"credential\": \"" <> ccHotCIP0129 <> "\""
        , "\"legacy_credential\": \"" <> ccHotCIP0105 <> "\""
        ]

    specInspect scriptHashGenericHex
        [ "\"role\": \"unknown\""
        , "\"script_hash\": \"" <> scriptHashGenericHex <> "\""
        , "\"credential\": \"" <> scriptHashGeneric <> "\""
        ]

    specInspect (drepHeader <> drepHex)
        [ "\"role\": \"representative\""
        , "\"script_hash\": \"" <> drepHex <> "\""
        , "\"credential\": \"" <> drepCIP0129 <> "\""
        , "\"legacy_credential\": \"" <> drepCIP0105 <> "\""
        ]

    specInspectInvalid "abcd" "Invalid script hash size"
    specInspectInvalid
        "0033445566778899001122334455667788990011223344556677889900"
        "Couldn't convert bytes into a CIP-0129 credential"
    specInspectInvalid
        "addr_vkh1dnup7vsddkycsau5ek5ulrhndastkxczpydp6jsmr8fgyfevcp3"
        "Invalid human-readable prefix"

specInspect :: String -> [String] -> SpecWith ()
specInspect input needles = it (input <> " => inspect") $ do
    out <- cli [ "script", "inspect" ] input
    out `shouldContain` "\"script_hash\""
    mapM_ (out `shouldContain`) needles

specInspectInvalid :: String -> String -> SpecWith ()
specInspectInvalid input errMsg = it (input <> " => error") $ do
    (out, err) <- cli [ "script", "inspect" ] input
    out `shouldBe` ""
    err `shouldContain` errMsg

drepHeader :: String
drepHeader = "23"

scriptHashGeneric :: String
scriptHashGeneric = "script1ugvvxx2vrajnx7q4y8a3mtjgrms8a3c85zz5hjvwa2gpqpkhpzq"

scriptHashGenericHex :: String
scriptHashGenericHex = "e218c3194c1f6533781521fb1dae481ee07ec707a0854bc98eea9010"

-- Vectors from
-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md
drepCIP0129 :: String
drepCIP0129 = "drep1ywxlnmh29f0jdv0uj2m5gtqgvgf0e5mtyajy0p2jf55tppszrkgvs"

drepCIP0105 :: String
drepCIP0105 = "drep_script13hu7a632tuntrlyjkazzczrzzt7dx6e8v3rc25jd9zcgv68nx9r"

drepHex :: String
drepHex = "8df9eeea2a5f26b1fc92b7442c086212fcd36b27644785524d28b086"

ccColdCIP0129 :: String
ccColdCIP0129 = "cc_cold1z0ty5s85w82gqy9599yltjnca6nas83xjjaw66nqv3j2j4cxpgzfw"

ccColdCIP0105 :: String
ccColdCIP0105 = "cc_cold_script16e9ypar36jqppdpff86u578w5lvpuf55htkk5cryvj54wd8s4jn"

ccColdHex :: String
ccColdHex = "d64a40f471d48010b42949f5ca78eea7d81e2694baed6a606464a957"

ccHotCIP0129 :: String
ccHotCIP0129 = "cc_hot1qday89cxvm00rceu70zsny95n485l50rh3pxy8k8m7yrwdgrehyfq"

ccHotCIP0105 :: String
ccHotCIP0105 = "cc_hot_script10fpewpnxmmc7x08nc5yepdyafa8arcaugf3pa37l3qmn2xu0u3c"

ccHotHex :: String
ccHotHex = "7a43970666def1e33cf3c50990b49d4f4fd1e3bc42621ec7df883735"
