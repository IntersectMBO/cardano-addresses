{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
    ( Depth (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , XPrv
    , XPub
    , toXPub
    , xpubFromBytes
    )
import Cardano.Address.Script
    ( Cosigner (..)
    , KeyHash (..)
    , Script (..)
    , ScriptHash (..)
    , ScriptTemplate (..)
    , serializeScript
    , toScriptHash
    , validateScriptTemplate
    )
import Cardano.Address.Script.Parser
    ( ErrValidateScript (..), validateScript )
import Cardano.Address.Style.Shelley
    ( Shelley (..), deriveMultisigPrivateKey, hashKey )
import Cardano.Mnemonic
    ( mkSomeMnemonic )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode, fromBase16 )
import Data.Aeson
    ( FromJSON, ToJSON )
import Data.Either
    ( isLeft )
import Data.Maybe
    ( fromJust )
import Data.Text
    ( Text )
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
import qualified Data.List as L
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

        it "invalid timelocks - too many" $ do
            let script = RequireSomeOf 1 [verKeyHash1, ActiveFromSlot 1, ActiveFromSlot 2, ActiveUntilSlot 20]
            validateScript script `shouldBe` (Left InvalidTimelocks)

        it "invalid timelocks - contradictory 1" $ do
            let script = RequireSomeOf 1 [verKeyHash1, ActiveFromSlot 21, ActiveUntilSlot 20]
            validateScript script `shouldBe` (Left InvalidTimelocks)

        it "invalid timelocks - contradictory 1" $ do
            let script = RequireSomeOf 1
                    [ verKeyHash1
                    , RequireAnyOf [ verKeyHash2
                                   , RequireSomeOf 2 [verKeyHash3, verKeyHash4, ActiveFromSlot 21, ActiveUntilSlot 20, verKeyHash1]
                                   ]
                    ]
            validateScript script `shouldBe` (Left InvalidTimelocks)

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

    describe "validateScriptTemplate - errors" $ do
        let accXpub0 =
                "7eebe6dfa9a1530248400eb6a1adaca166ab1d723e9618d989d22a9219a364\
                \cb4c745e128fdc98a5039893f704cf67f58c59cea97241a5c7ec7b4606253e5523"
        let accXpub1 =
                "417236c94b3ad73557a4df690527f77bebd203de7a208fb3be9c5efa675aaa\
                \967ca13a50a2f2e95364d0b7fdc75a82e8cc97b499ecd6b9ba12529dd63a2ca7d5"
        let accXpub2 =
                "ebf69a16263b741240d3a3d67b44be3a70516adc1a7422b214d0e379314692\
                \9eb053c9d5500fdcc4088b6a2c3b20b145d84ca77d5ad59343ddf4ba6c9b482d7c"
        let accXpub3 =
                "30a71e7919e9c409811efe8d818b831096ac44678397e8911c921a19f2e9b7\
                \f45b45a93ec2432ed0d314e356a69409c21823f152ae898a97b9b6f72ecd9c2400"
        let cosigners' = Map.fromList $
                zipWith (\ix accXpub -> (Cosigner ix, encodeXPubFromTxtUnsafe accXpub))
                [0, 1, 2, 3] [accXpub0, accXpub1, accXpub2, accXpub3]
        let cosignersWrong = Map.fromList $
                zipWith (\ix accXpub -> (Cosigner ix, encodeXPubFromTxtUnsafe accXpub))
                [0, 1, 2, 3] [accXpub0, accXpub1, accXpub2, accXpub0]
        let cosigner0 = RequireSignatureOf (Cosigner 0)
        let cosigner1 = RequireSignatureOf (Cosigner 1)
        let cosigner2 = RequireSignatureOf (Cosigner 2)
        let cosigner3 = RequireSignatureOf (Cosigner 3)

        it "no cosigners in script template" $ do
            let scriptTemplate = ScriptTemplate Map.empty (RequireAllOf [])
            validateScriptTemplate scriptTemplate `shouldBe` (Left NoCosigner)

        it "illegal cosigner in script template" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSignatureOf (Cosigner 4))
            validateScriptTemplate scriptTemplate `shouldBe` (Left UnknownCosigner)

        it "duplicated xpub in cosigners in script template" $ do
            let scriptTemplate = ScriptTemplate cosignersWrong (RequireSignatureOf (Cosigner 1))
            validateScriptTemplate scriptTemplate `shouldBe` (Left DuplicateXPubs)

        it "no content in RequireAnyOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAnyOf [])
            validateScriptTemplate scriptTemplate `shouldBe` (Left EmptyList)

        it "no content in RequireSomeOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1 [])
            validateScriptTemplate scriptTemplate `shouldBe` (Left ListTooSmall)

        it "too high m in RequireSomeOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 5 [cosigner0, cosigner1, cosigner2, cosigner3])
            validateScriptTemplate scriptTemplate `shouldBe` (Left ListTooSmall)

        it "m=0 in RequireSomeOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 0 [cosigner0, cosigner1, cosigner2, cosigner3])
            validateScriptTemplate scriptTemplate `shouldBe`(Left MZero)

        it "wrong in nested 1" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1 [cosigner0, cosigner1, cosigner2, cosigner3, RequireAnyOf [] ])
            validateScriptTemplate scriptTemplate `shouldBe` (Left EmptyList)

        it "wrong in nested 2" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner0, cosigner1, cosigner2, cosigner3
                    , RequireAnyOf [cosigner2, RequireAllOf [] ]
                    ])
            validateScriptTemplate scriptTemplate `shouldBe` (Left EmptyList)

        it "wrong in nested 3" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner0, cosigner1, cosigner2, cosigner3
                    , RequireAnyOf [ cosigner2
                                   , RequireSomeOf 3 [cosigner0, cosigner3]
                                   ]
                    ])
            validateScriptTemplate scriptTemplate `shouldBe` (Left ListTooSmall)

        it "duplicate content in RequireAllOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAllOf [cosigner1, cosigner2, cosigner1])
            validateScriptTemplate scriptTemplate `shouldBe` (Left DuplicateSignatures)

        it "duplicate content in RequireAnyOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAnyOf [cosigner1, cosigner2, cosigner1])
            validateScriptTemplate scriptTemplate `shouldBe` (Left DuplicateSignatures)

        it "duplicate content in RequireSomeOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1 [cosigner1, cosigner2, cosigner1])
            validateScriptTemplate scriptTemplate `shouldBe` (Left DuplicateSignatures)

        it "duplicate in nested" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner1
                    , RequireAnyOf [ cosigner2
                                   , RequireSomeOf 2 [cosigner0, cosigner0, cosigner3]
                                   ]
                    ])
            validateScriptTemplate scriptTemplate `shouldBe` (Left DuplicateSignatures)

        it "invalid timelocks - too many" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1 [cosigner0, ActiveFromSlot 1, ActiveFromSlot 2, ActiveUntilSlot 20])
            validateScriptTemplate scriptTemplate `shouldBe` (Left InvalidTimelocks)

        it "invalid timelocks - contradictory 1" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1 [cosigner0, ActiveFromSlot 21, ActiveUntilSlot 20])
            validateScriptTemplate scriptTemplate `shouldBe` (Left InvalidTimelocks)

        it "invalid timelocks - contradictory 1" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner1
                    , RequireAnyOf [ cosigner2
                                   , RequireSomeOf 2 [cosigner0, cosigner1, ActiveFromSlot 21, ActiveUntilSlot 20, cosigner3]
                                   ]
                    ])
            validateScriptTemplate scriptTemplate `shouldBe` (Left InvalidTimelocks)

        it "too high m in RequireSomeOf when timelocks" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 3 [cosigner0, cosigner1, ActiveFromSlot 21, ActiveUntilSlot 30])
            validateScriptTemplate scriptTemplate `shouldBe` (Left ListTooSmall)

        it "no content in RequireAnyOf when timelocks" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAnyOf [ActiveFromSlot 21, ActiveUntilSlot 30])
            validateScriptTemplate scriptTemplate `shouldBe` (Left EmptyList)

        it "no content in RequireAnyOf when timelocks" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAnyOf [ActiveFromSlot 21, ActiveUntilSlot 30])
            validateScriptTemplate scriptTemplate `shouldBe` (Left EmptyList)

    describe "can perform roundtrip JSON serialization & deserialization - Script KeyHash" $
        it "fromJSON . toJSON === pure" $ property (prop_jsonRoundtrip @(Script KeyHash))
    describe "can perform roundtrip JSON serialization & deserialization - Script KeyHash validated" $
        it "fromJSON . toJSON === pure" $ property (prop_jsonRoundtripWithValidation validateScript)
    describe "can perform roundtrip JSON serialization & deserialization - Script Cosigner" $
        it "fromJSON . toJSON === pure" $ property (prop_jsonRoundtrip @(Script Cosigner))
    describe "can perform roundtrip JSON serialization & deserialization - ScriptTemplate" $
        it "fromJSON . toJSON === pure" $ property (prop_jsonRoundtrip @ScriptTemplate)
    describe "can perform roundtrip JSON serialization & deserialization - ScriptTemplate validated" $
        it "fromJSON . toJSON === pure" $ property (prop_jsonRoundtripWithValidation validateScriptTemplate)

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

prop_jsonRoundtripWithValidation
    :: (Eq a, Show a, ToJSON a, FromJSON a)
    => (a -> Either ErrValidateScript ())
    -> a
    -> Property
prop_jsonRoundtripWithValidation validate script =
    classify (isLeft $ validate script) "invalid" $
    Json.decode (Json.encode script) === Just script

prop_jsonRoundtrip :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> Property
prop_jsonRoundtrip val =
    Json.decode (Json.encode val) === Just val

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
            let hasTimelocks = \case
                    ActiveFromSlot _ -> True
                    ActiveUntilSlot _ -> True
                    _ -> False
            let scriptsWithValidTimelocks = case L.partition hasTimelocks scripts of
                    ([], rest) -> rest
                    ([ActiveFromSlot s1, ActiveUntilSlot s2], rest) ->
                        if s2 <= s1 then
                            rest ++ [ActiveFromSlot s2, ActiveUntilSlot s1]
                        else
                            scripts
                    ([ActiveUntilSlot s2, ActiveFromSlot s1], rest) ->
                        if s2 <= s1 then
                            rest ++ [ActiveFromSlot s2, ActiveUntilSlot s1]
                        else
                            scripts
                    ([ActiveFromSlot _], _) -> scripts
                    ([ActiveUntilSlot _], _) -> scripts
                    (_,rest) -> rest
            case fromIntegral (L.length (filter (not . hasTimelocks) scriptsWithValidTimelocks)) of
                0 -> scriptTree 0
                num -> do
                    atLeast <- choose (1, num)
                    elements
                        [ RequireAllOf scriptsWithValidTimelocks
                        , RequireAnyOf scriptsWithValidTimelocks
                        , RequireSomeOf atLeast scriptsWithValidTimelocks
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

encodeXPubFromTxtUnsafe :: Text -> XPub
encodeXPubFromTxtUnsafe txt =
        case fromBase16 (T.encodeUtf8 txt) of
            Left _ -> error "encodeXPubFromTxtUnsafe: expecting hex-encoded text"
            Right hex -> fromJust $ xpubFromBytes hex
