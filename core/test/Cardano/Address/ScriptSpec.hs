{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Address.ScriptSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..), GenMasterKey (..), HardDerivation (..), XPrv, XPub, toXPub )
import Cardano.Address.Script
    ( Cosigner (..)
    , KeyHash (..)
    , Script (..)
    , ScriptHash (..)
    , ScriptTemplate (..)
    , serializeScript
    , toScriptHash
    )
import Cardano.Address.Script.Parser
    ( ErrValidateScript (..), validateScript )
import Cardano.Address.Style.Shelley
    ( Shelley (..), deriveMultisigPrivateKey, hashKey )
import Cardano.Mnemonic
    ( mkSomeMnemonic )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Data.Aeson
    ( FromJSON, ToJSON )
import Data.Either
    ( isLeft )
import Test.Arbitrary
    ()
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Positive (..)
    , Property
    , choose
    , classify
    , elements
    , genericShrink
    , oneof
    , property
    , scale
    , sized
    , vectorOf
    , (===)
    )

import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    let mnemonic = [ "test", "child", "burst", "immense", "armed", "parrot"
                   , "company", "walk", "dog" ]
    let (Right mw) = mkSomeMnemonic @'[9,12,15,18,21,24] mnemonic
    let sndFactor = mempty
    let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    let accXPrv = deriveAccountPrivateKey rootK minBound

    let index1 = minBound
    let multisigXPub1 = toXPub <$> deriveMultisigPrivateKey accXPrv index1
    -- "deeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e"
    let verKeyHash1 = RequireSignatureOf $ hashKey multisigXPub1

    let index2 = toEnum 0x00000001
    let multisigXPub2 = toXPub <$> deriveMultisigPrivateKey accXPrv index2
    -- "60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
    let verKeyHash2 = RequireSignatureOf $ hashKey multisigXPub2

    let index3 = toEnum 0x00000002
    let multisigXPub3 = toXPub <$> deriveMultisigPrivateKey accXPrv index3
    -- "ffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
    let verKeyHash3 = RequireSignatureOf $ hashKey multisigXPub3

    let index4 = toEnum 0x00000003
    let multisigXPub4 = toXPub <$> deriveMultisigPrivateKey accXPrv index4
    -- "96834025cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"
    let verKeyHash4 = RequireSignatureOf $ hashKey multisigXPub4

    describe "Multisig CBOR and hashes - golden tests" $ do
        let checkCBORandScriptHash script cbor hash = do
                (toHexText (serializeScript script)) `shouldBe` cbor
                (toHexText' (toScriptHash script)) `shouldBe` hash

        it "RequireSignatureOf index=0" $
            checkCBORandScriptHash verKeyHash1
                "008200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e"
                "59fd497a34ac3e5abf2c8a703e3aaf3a2750e207b139d65d08d2c1b3"
        it "RequireSignatureOf index=1" $
            checkCBORandScriptHash verKeyHash2
                "008200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
                "8d60cfd18163231751491389db7fa95bbb4192452d493f4147949f42"
        it "RequireSignatureOf index=2" $
            checkCBORandScriptHash verKeyHash3
                "008200581cffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
                "f34b7c6642ee2d2ff115a09d60d084d8df866c1be7722bfb78585d75"
        it "RequireSignatureOf index=3" $
            checkCBORandScriptHash verKeyHash4
                "008200581c96834025cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"
                "324f9577142f2034545ba2e905d6e8a18afbd3ede08ec6c274aa641b"

        it "RequireAllOf for index=0 and index=1 keys" $ do
            let script = RequireAllOf [verKeyHash1, verKeyHash2]
            checkCBORandScriptHash script
                "008201828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a50437\
                \9cfc1e8200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
                "57eec18300169c459169e335b809dbf50ca236d6ef730f623799a004"
        it "RequireAllOf for index=0, index=1 and index=2 keys" $ do
            let script = RequireAllOf [verKeyHash1, verKeyHash2, verKeyHash3]
            checkCBORandScriptHash script
                "008201838200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8200\
                \581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581cffcbb72393\
                \215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
                "3db6ba0c234043ab963f5cc723d8a953d46477dbdc45f3dbd73847f1"

        it "RequireAnyOf for index=0 and index=1 keys" $ do
            let script = RequireAnyOf [verKeyHash1, verKeyHash2]
            checkCBORandScriptHash script
                "008202828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a50437\
                \9cfc1e8200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
                "ee98a536acf306c398d0b51a675a088faf76d7e41d1f0ab94efc3be7"
        it "RequireAllOf for index=0, index=1 and index=2 keys" $ do
            let script = RequireAnyOf [verKeyHash1, verKeyHash2, verKeyHash3]
            checkCBORandScriptHash script
                "008202838200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8200\
                \581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581cffcbb72393\
                \215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
                "270cbddf1d43fb4ad7eca05f08f2c9c65a290389d8c48c57ba9f38c4"

        it "RequireSomeOf 1 out of index=0 and index=1 keys" $ do
            let script = RequireSomeOf 1 [verKeyHash1, verKeyHash2]
            checkCBORandScriptHash script
                "00830301828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504\
                \379cfc1e8200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
                "31aa5030ae386603145f0cb16577da64ce0647b3cf2104e8d5646d67"
        it "RequireAllOf 2 out of index=0, index=1, index=2 and index=3 keys" $ do
            let script = RequireSomeOf 2 [verKeyHash1, verKeyHash2, verKeyHash3, verKeyHash4]
            checkCBORandScriptHash script
                "00830302848200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e82\
                \00581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581cffcbb723\
                \93215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c96834025cdca063ce9c32d\
                \fae6bc6a3e47f8da07ee4fb8e1a3901559"
                "e2da1830b3465ae1a9161e89ff79673d8d133c841ab06418f0034534"

        it "nested 1" $ do
            let nested = RequireAllOf [verKeyHash3, verKeyHash4]
            let script = RequireSomeOf 2 [verKeyHash1, verKeyHash2, nested]
            checkCBORandScriptHash script
                "00830302838200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8\
                \200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8201828200581c\
                \ffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c96834025cdca0\
                \63ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"
                "1ae2515f480c8b67fc2fcccab565bbc12b196a24083f6cf278c3ed7a"

        it "nested 2" $ do
            let nested = RequireAnyOf [verKeyHash2, verKeyHash3, verKeyHash4]
            let script = RequireAllOf [verKeyHash1, nested]
            checkCBORandScriptHash script
                "008201828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e82\
                \02838200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581c\
                \ffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c96834025cdca\
                \063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"
                "ae743bad455bad082ab69fd31183fb70a2787274938c85048b80e8ee"

        it "nested 3" $ do
            let nested' = RequireAnyOf [verKeyHash3, verKeyHash4]
            let nested = RequireAllOf [verKeyHash1, nested']
            let script = RequireSomeOf 1 [verKeyHash1, nested]
            checkCBORandScriptHash script
                "00830301828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8\
                \201828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e82028282\
                \00581cffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c9683402\
                \5cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"
                "8aa7af44362310140ff3d32ac7d1a2ecbe26da65f3d146c64b90e9e1"

        it "ActivateFromSlot" $ do
            let script = RequireAllOf [verKeyHash1, ActiveFromSlot 120]
            checkCBORandScriptHash script
                "008201828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e82041878"
                "f0252f0e8b31694afe2e793aefa8420fe7cdefc08003fe6a911f710d"

        it "ActivateUntilSlot" $ do
            let script = RequireAllOf [verKeyHash1, ActiveUntilSlot 150]
            checkCBORandScriptHash script
                "008201828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e82051896"
                "88bf6eca7c3bf7abc74fdb6bd8dee2ac08656a9b79bd5cccfddda058"

        it "ActivateUntilSlot and ActivateUntilSlot" $ do
            let script = RequireAllOf [verKeyHash1, ActiveFromSlot 120, ActiveUntilSlot 150]
            checkCBORandScriptHash script
                "008201838200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8204187882051896"
                "344ff9c219027af44c14c1d7c47bdf4c14b95c93739cac0b03c41b76"

    describe "validateScript - errors" $ do
        it "no content in RequireAllOf" $ do
            let script = RequireAllOf []
            validateScript script `shouldBe` (Left EmptyList)

        it "no content in RequireAnyOf" $ do
            let script = RequireAnyOf []
            validateScript script `shouldBe` (Left EmptyList)

        it "no content in RequireSomeOf" $ do
            let script = RequireSomeOf 1 []
            validateScript script `shouldBe` (Left ListTooSmall)

        it "too high m in RequireSomeOf" $ do
            let script = RequireSomeOf 3 [verKeyHash3, verKeyHash4]
            validateScript script `shouldBe` (Left ListTooSmall)

        it "m=0 in RequireSomeOf" $ do
            let script = RequireSomeOf 0 [verKeyHash3, verKeyHash4]
            validateScript script `shouldBe`(Left MZero)

        it "wrong in nested 1" $ do
            let script = RequireSomeOf 1 [verKeyHash1, RequireAnyOf [] ]
            validateScript script `shouldBe` (Left EmptyList)

        it "wrong in nested 2" $ do
            let script = RequireSomeOf 1
                    [ verKeyHash1
                    , RequireAnyOf [verKeyHash2, RequireAllOf [] ]
                    ]
            validateScript script `shouldBe` (Left EmptyList)

        it "wrong in nested 3" $ do
            let script = RequireSomeOf 1
                    [ verKeyHash1
                    , RequireAnyOf [ verKeyHash2
                                   , RequireSomeOf 3 [verKeyHash3, verKeyHash4]
                                   ]
                    ]
            validateScript script `shouldBe` (Left ListTooSmall)

        it "duplicate content in RequireAllOf" $ do
            let script = RequireAllOf [verKeyHash1, verKeyHash2, verKeyHash1]
            validateScript script `shouldBe` (Left DuplicateSignatures)

        it "duplicate content in RequireAnyOf" $ do
            let script = RequireAnyOf [verKeyHash1, verKeyHash2, verKeyHash1]
            validateScript script `shouldBe` (Left DuplicateSignatures)

        it "duplicate content in RequireSomeOf" $ do
            let script = RequireSomeOf 1 [verKeyHash1, verKeyHash2, verKeyHash1]
            validateScript script `shouldBe` (Left DuplicateSignatures)

        it "duplicate in nested" $ do
            let script = RequireSomeOf 1
                    [ verKeyHash1
                    , RequireAnyOf [ verKeyHash2
                                   , RequireSomeOf 2 [verKeyHash3, verKeyHash3, verKeyHash4]
                                   ]
                    ]
            validateScript script `shouldBe` (Left DuplicateSignatures)

    describe "validateScript - correct" $ do
        it "content in RequireAllOf - 1" $ do
            let script = RequireAllOf [verKeyHash1]
            validateScript script `shouldBe` Right ()

        it "content in RequireAllOf - 2" $ do
            let script = RequireAllOf [verKeyHash1, verKeyHash2]
            validateScript script `shouldBe` Right ()

        it "nested 1" $ do
            let script = RequireSomeOf 1
                    [ verKeyHash1
                    , RequireAnyOf
                        [ verKeyHash2
                        , RequireSomeOf 1 [verKeyHash3, verKeyHash4]
                        ]
                    ]
            validateScript script `shouldBe` Right ()

        it "nested 2" $ do
            let script = RequireSomeOf 1
                    [ RequireAnyOf
                        [ verKeyHash1
                        , verKeyHash2
                        ]
                    , RequireAnyOf
                        [ verKeyHash1
                        , verKeyHash3
                        ]
                    ]
            validateScript script `shouldBe` Right ()

    describe "can perform roundtrip JSON serialization & deserialization - Script KeyHash" $
        it "fromJSON . toJSON === pure" $ property prop_jsonRoundtripWithValidation
    describe "can perform roundtrip JSON serialization & deserialization - Script Cosigner" $
        it "fromJSON . toJSON === pure" $ property (prop_jsonRoundtrip @(Script Cosigner))
    describe "can perform roundtrip JSON serialization & deserialization - ScriptTemplate" $
        it "fromJSON . toJSON === pure" $ property (prop_jsonRoundtrip @ScriptTemplate)
    describe "can perform roundtrip JSON serialization & deserialization - XPub" $
        it "fromJSON . toJSON === pure" $ property (prop_jsonRoundtrip @XPub)

    describe "some JSON parsing error" $ do
        it "Empty list" $ do
            let err = "Error in $: The list inside a script is empty."
            Json.eitherDecode @(Script KeyHash) "{ \"any\": [] }"
                `shouldBe` Left err

        it "Empty list nested" $ do
            let err = "Error in $.any[0]: The list inside a script is empty."
            Json.eitherDecode @(Script KeyHash) "{ \"any\": [ { \"all\": [] } ] }"
                `shouldBe` Left err

        it "Invalid type" $ do
            let err = "Error in $.all[0].any[0]: expected Object or String, but encountered Number"
            Json.eitherDecode @(Script KeyHash) "{ \"all\": [ { \"any\": [1,2,3] } ] }"
                `shouldBe` Left err

        it "List too small" $ do
            let err = "Error in $: At least must not be larger than the list of keys."
            Json.eitherDecode @(Script KeyHash)
                "{ \"some\":\
                \   { \"at_least\": 2\
                \   , \"from\":\
                \       [\"script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyreluzt36ms\"\
                \       ]\
                \   }\
                \}" `shouldBe` Left err

        it "Multiple 'any', 'all'" $ do
            let err = "Error in $.all[0].any[0]: expected Object or String, but encountered Number"
            Json.eitherDecode @(Script KeyHash) "{ \"all\": [ { \"any\": [1,2,3] } ] }"
                `shouldBe` Left err

        it "Multiple keys" $ do
            let err = "Error in $: Found multiple keys 'any', 'all' and/or 'some' at the same level"
            Json.eitherDecode @(Script KeyHash) "{ \"all\": [null], \"some\": {} }"
                `shouldBe` Left err

        it "Unknown keys" $ do
            let err = "Error in $: Found object with no known key 'any', 'all' or 'some'"
            Json.eitherDecode @(Script KeyHash) "{ \"patate\": {} }"
                `shouldBe` Left err

        it "Invalid JSON" $ do
            let err = "Error in $: Failed reading: not a valid json value"
            Json.eitherDecode @(Script KeyHash) "'';[]["
                `shouldBe` Left err
  where
    toHexText = T.decodeUtf8 . encode EBase16
    toHexText' (ScriptHash bytes) = toHexText bytes

prop_jsonRoundtripWithValidation :: Script KeyHash -> Property
prop_jsonRoundtripWithValidation script =
    classify (isLeft $ validateScript script) "invalid" $
    roundtrip script === Just script
  where
    roundtrip :: Script KeyHash -> Maybe (Script KeyHash)
    roundtrip = Json.decode . Json.encode

prop_jsonRoundtrip :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> Property
prop_jsonRoundtrip val =
    (Json.decode $ Json.encode val) === Just val

instance Arbitrary (Script KeyHash) where
    arbitrary = genScript (RequireSignatureOf <$> arbitrary)
    shrink = genericShrink

instance Arbitrary (Script Cosigner) where
    arbitrary = genScript (RequireSignatureOf <$> arbitrary)
    shrink = genericShrink

genScript :: Gen (Script elem) -> Gen (Script elem)
genScript elemGen = scale (`div` 3) $ sized scriptTree
    where
        scriptTree 0 = oneof
            [ elemGen
            , ActiveFromSlot <$> arbitrary
            , ActiveUntilSlot <$> arbitrary
            ]
        scriptTree n = do
            Positive m <- arbitrary
            let n' = n `div` (m + 1)
            scripts <- vectorOf m (scriptTree n')
            atLeast <- choose (1, fromIntegral m)
            elements
                [ RequireAllOf scripts
                , RequireAnyOf scripts
                , RequireSomeOf atLeast scripts
                ]

instance Arbitrary KeyHash where
    -- always generate valid hashes, because json decoding will immediately fail
    -- on these.
    arbitrary = KeyHash . BS.pack <$> vectorOf 28 arbitrary

instance Arbitrary Cosigner where
    arbitrary = Cosigner <$> arbitrary

instance Arbitrary ScriptTemplate where
    arbitrary = do
        n <- choose (1,5)
        cosignerPairs <- vectorOf n arbitrary
        ScriptTemplate (Map.fromList cosignerPairs) <$> arbitrary
