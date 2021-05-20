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
    , DerivationType (..)
    , Index
    , XPrv
    , XPub
    , indexFromWord32
    , toXPub
    , xpubFromBytes
    )
import Cardano.Address.Script
    ( Cosigner (..)
    , ErrRecommendedValidateScript (..)
    , ErrValidateScript (..)
    , ErrValidateScriptTemplate (..)
    , KeyHash (..)
    , KeyRole (..)
    , Script (..)
    , ScriptHash (..)
    , ScriptTemplate (..)
    , ValidationLevel (..)
    , serializeScript
    , toScriptHash
    , validateScript
    , validateScriptInScriptTemplate
    , validateScriptTemplate
    )
import Cardano.Address.Style.Shared
    ( Shared (..), hashKey )
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
    ( Spec, describe, expectationFailure, it, shouldBe, shouldStartWith )
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

import qualified Cardano.Address.Style.Shared as Shared
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    let mnemonic = ["network","empty","cause","mean","expire","private","finger"
                   ,"accident","session","problem","absurd","banner","stage","void","what"]
    let (Right mw) = mkSomeMnemonic @'[9,15,18,21,24] mnemonic
    let sndFactor = mempty
    let rootK = Shared.genMasterKeyFromMnemonic mw sndFactor :: Shared 'RootK XPrv
    let accXPrv = Shared.deriveAccountPrivateKey rootK minBound

    let index1 = minBound
    let multisigXPub1 = toXPub <$> Shared.deriveAddressPrivateKey accXPrv index1
    let verKeyHash1 = RequireSignatureOf $ hashKey Payment multisigXPub1

    let Just index2 = indexFromWord32 @(Index 'Soft _) 0x00000001
    let multisigXPub2 = toXPub <$> Shared.deriveAddressPrivateKey accXPrv index2
    let verKeyHash2 = RequireSignatureOf $ hashKey Payment multisigXPub2

    let Just index3 = indexFromWord32 @(Index 'Soft _) 0x00000002
    let multisigXPub3 = toXPub <$> Shared.deriveAddressPrivateKey accXPrv index3
    let verKeyHash3 = RequireSignatureOf $ hashKey Payment multisigXPub3

    let Just index4 = indexFromWord32 @(Index 'Soft _) 0x00000003
    let multisigXPub4 = toXPub <$> Shared.deriveAddressPrivateKey accXPrv index4
    let verKeyHash4 = RequireSignatureOf $ hashKey Payment multisigXPub4

    let multisigXPub5 = toXPub <$> Shared.deriveDelegationPrivateKey accXPrv index4
    let verKeyHash5 = RequireSignatureOf $ hashKey Delegation multisigXPub5

    describe "Multisig CBOR and hashes - golden tests" $ do
        let checkCBORandScriptHash script cbor hash = do
                (toHexText (serializeScript script)) `shouldBe` cbor
                (toHexText' (toScriptHash script)) `shouldBe` hash

        it "RequireSignatureOf index=0" $
            checkCBORandScriptHash verKeyHash1
                "008200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab0986ce33d2"
                "2e768befb6e18e302a5d89fc4d4bcbd5d63ac256682f87246c06b1bb"
        it "RequireSignatureOf index=1" $
            checkCBORandScriptHash verKeyHash2
                "008200581c91e3548bc14b2947014d2060fe17eaf172b85efb02323752400976cf"
                "7b244224d7e6517ceb99bd2f83ccc4587285f276acb9caa484c9c306"
        it "RequireSignatureOf index=2" $
            checkCBORandScriptHash verKeyHash3
                "008200581c92d19fcb8f67c609f43aef8c53a2226dd141866820ebaa5157479101"
                "98530ec66e36061feabcf6b3ef730b75cf8bf13b9948506382272dc7"
        it "RequireSignatureOf index=3" $
            checkCBORandScriptHash verKeyHash4
                "008200581c1eb1bcd2ebea2641d31e2be9b3db5fd9bd2c54a5d11c2a5f1d08c85b"
                "547794e90ad105ea6dc75d0bac54c1473c663b7396dfc82fc34c5a9d"

        it "RequireAllOf for index=0 and index=1 keys" $ do
            let script = RequireAllOf [verKeyHash1, verKeyHash2]
            checkCBORandScriptHash script
                "008201828200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab098\
                \6ce33d28200581c91e3548bc14b2947014d2060fe17eaf172b85efb02323752400976cf"
                "0334c66ba3bada99c0bda26f88b8a1aea8c0aba87956d24e938a0f48"
        it "RequireAllOf for index=0, index=1 and index=2 keys" $ do
            let script = RequireAllOf [verKeyHash1, verKeyHash2, verKeyHash3]
            checkCBORandScriptHash script
                "008201838200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab098\
                \6ce33d28200581c91e3548bc14b2947014d2060fe17eaf172b85efb0232375240\
                \0976cf8200581c92d19fcb8f67c609f43aef8c53a2226dd141866820ebaa5157479101"
                "d5913dca5953a9bba712fde99b4ebc59ac83f5734f73d0c3c528450d"

        it "RequireAnyOf for index=0 and index=1 keys" $ do
            let script = RequireAnyOf [verKeyHash1, verKeyHash2]
            checkCBORandScriptHash script
                "008202828200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab098\
                \6ce33d28200581c91e3548bc14b2947014d2060fe17eaf172b85efb02323752400976cf"
                "b17721daca904785d6f610df2d22dd949f0698561cd0463962bbcfa7"
        it "RequireAllOf for index=0, index=1 and index=2 keys" $ do
            let script = RequireAnyOf [verKeyHash1, verKeyHash2, verKeyHash3]
            checkCBORandScriptHash script
                "008202838200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab098\
                \6ce33d28200581c91e3548bc14b2947014d2060fe17eaf172b85efb0232375240\
                \0976cf8200581c92d19fcb8f67c609f43aef8c53a2226dd141866820ebaa5157479101"
                "2149c366b64f75cf17dc25c77576e906b92d780bef9a985f9ecc9193"

        it "RequireSomeOf 1 out of index=0 and index=1 keys" $ do
            let script = RequireSomeOf 1 [verKeyHash1, verKeyHash2]
            checkCBORandScriptHash script
                "00830301828200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab098\
                \6ce33d28200581c91e3548bc14b2947014d2060fe17eaf172b85efb02323752400976cf"
                "8f81fd46f3fabb58dc2d72a31e239740dd5e09d268e752f11deeb460"
        it "RequireAllOf 2 out of index=0, index=1, index=2 and index=3 keys" $ do
            let script = RequireSomeOf 2 [verKeyHash1, verKeyHash2, verKeyHash3, verKeyHash4]
            checkCBORandScriptHash script
                "00830302848200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab098\
                \6ce33d28200581c91e3548bc14b2947014d2060fe17eaf172b85efb023237524009\
                \76cf8200581c92d19fcb8f67c609f43aef8c53a2226dd141866820ebaa515747910\
                \18200581c1eb1bcd2ebea2641d31e2be9b3db5fd9bd2c54a5d11c2a5f1d08c85b"
                "ded625e186392b3dd432824a51bf4aedad188236ae3d475ee634c560"

        it "nested 1" $ do
            let nested = RequireAllOf [verKeyHash3, verKeyHash4]
            let script = RequireSomeOf 2 [verKeyHash1, verKeyHash2, nested]
            checkCBORandScriptHash script
                "00830302838200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab098\
                \6ce33d28200581c91e3548bc14b2947014d2060fe17eaf172b85efb023237524009\
                \76cf8201828200581c92d19fcb8f67c609f43aef8c53a2226dd141866820ebaa515\
                \74791018200581c1eb1bcd2ebea2641d31e2be9b3db5fd9bd2c54a5d11c2a5f1d08c85b"
                "3e4bdfb98e8e83a77350dbb546222dc9dec59a31fa413207270e60ea"

        it "nested 2" $ do
            let nested = RequireAnyOf [verKeyHash2, verKeyHash3, verKeyHash4]
            let script = RequireAllOf [verKeyHash1, nested]
            checkCBORandScriptHash script
                "008201828200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab098\
                \6ce33d28202838200581c91e3548bc14b2947014d2060fe17eaf172b85efb0232\
                \3752400976cf8200581c92d19fcb8f67c609f43aef8c53a2226dd141866820eba\
                \a51574791018200581c1eb1bcd2ebea2641d31e2be9b3db5fd9bd2c54a5d11c2a5f1d08c85b"
                "f21e0e095cee196d45796e9108bc2a9ebc0f81082850ad4b82ae92ff"

        it "nested 3" $ do
            let nested' = RequireAnyOf [verKeyHash3, verKeyHash4]
            let nested = RequireAllOf [verKeyHash1, nested']
            let script = RequireSomeOf 1 [verKeyHash1, nested]
            checkCBORandScriptHash script
                "00830301828200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab098\
                \6ce33d28201828200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab\
                \0986ce33d28202828200581c92d19fcb8f67c609f43aef8c53a2226dd141866820e\
                \baa51574791018200581c1eb1bcd2ebea2641d31e2be9b3db5fd9bd2c54a5d11c2a5f1d08c85b"
                "5cf0520d67e4ac52a47fdbf9efbb1a5b95a0df4595287bafdb099d5b"

        it "ActivateFromSlot" $ do
            let script = RequireAllOf [verKeyHash1, ActiveFromSlot 120]
            checkCBORandScriptHash script
                "008201828200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab0986ce33d282041878"
                "068bc5b5efb96ff9831d75a132de4b700f8b605ebce43b2f8e340305"

        it "ActivateUntilSlot" $ do
            let script = RequireAllOf [verKeyHash1, ActiveUntilSlot 150]
            checkCBORandScriptHash script
                "008201828200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab0986ce33d282051896"
                "83dee9daf83dd2dcff0b8e5aec72d256e05c283a1824e84485a0c726"

        it "ActivateUntilSlot and ActivateUntilSlot" $ do
            let script = RequireAllOf [verKeyHash1, ActiveFromSlot 120, ActiveUntilSlot 150]
            checkCBORandScriptHash script
                "008201838200581c87ae348a59e3559f84984cc9f85ede71b9f2227825ecab0986ce33d28204187882051896"
                "37a5c4fe748ae7f01b9f029c875cbb2b407c559fd497786022a57019"

    describe "validateScript - expectations for RequiredValidation" $ do
        it "incorrect RequireSignatureOf" $ do
            let script = RequireSignatureOf (KeyHash Payment "<wrong key hash>")
            validateScript RequiredValidation script `shouldBe` (Left WrongKeyHash)

        it "incorrect RequireSignatureOf nested" $ do
            let script = RequireAllOf [RequireAnyOf [ RequireSignatureOf (KeyHash Payment "<wrong key hash>")]]
            validateScript RequiredValidation script `shouldBe` (Left WrongKeyHash)

        it "correct RequireAllOf []" $ do
            let script = RequireAllOf []
            validateScript RequiredValidation script `shouldBe` (Right ())

        it "incorrect RequireAnyOf []" $ do
            let script = RequireAnyOf []
            validateScript RequiredValidation script `shouldBe` (Left LedgerIncompatible)

        it "incorrect RequireSomeOf 1" $ do
            let script = RequireSomeOf 2 [verKeyHash1]
            validateScript RequiredValidation script `shouldBe` (Left LedgerIncompatible)

        it "incorrect RequireSomeOf 2" $ do
            let script = RequireSomeOf 2 [verKeyHash1, verKeyHash1, RequireAnyOf [], RequireSignatureOf (KeyHash Payment "<wrong key hash>")]
            validateScript RequiredValidation script `shouldBe` (Left WrongKeyHash)

        it "correct RequireSomeOf" $ do
            let script = RequireSomeOf 2 [verKeyHash1, verKeyHash1, RequireAnyOf []]
            validateScript RequiredValidation script `shouldBe` Right ()

        it "m=0 in RequireSomeOf is correct" $ do
            let script = RequireSomeOf 0 [verKeyHash3, verKeyHash4]
            validateScript RequiredValidation script  `shouldBe` Right ()

        it "timelocks are correct if timelocks are disjoint" $ do
            let script = RequireSomeOf 2 [ActiveFromSlot 9, ActiveUntilSlot 8 ]
            validateScript RequiredValidation script `shouldBe` Right ()

    describe "validateScript - expectations for RecomendedValidation" $ do
        it "incorrect RequireAllOf []" $ do
            let script = RequireAllOf []
            validateScript RecommendedValidation script `shouldBe` Left (NotRecommended EmptyList)

        it "incorrect in nested 1" $ do
            let script = RequireSomeOf 1 [verKeyHash1, RequireAllOf [] ]
            validateScript RecommendedValidation script `shouldBe` Left (NotRecommended EmptyList)

        it "incorrect in nested 2" $ do
            let script = RequireSomeOf 1
                    [ verKeyHash1
                    , RequireAnyOf [verKeyHash2, RequireAllOf [] ]
                    ]
            validateScript RecommendedValidation script `shouldBe` Left (NotRecommended EmptyList)

        it "m=0 in RequireSomeOf" $ do
            let script = RequireSomeOf 0 [verKeyHash3, verKeyHash4]
            validateScript RecommendedValidation script `shouldBe` Left (NotRecommended MZero)

        it "duplicate content in RequireAllOf" $ do
            let script = RequireAllOf [verKeyHash1, verKeyHash2, verKeyHash1]
            validateScript RecommendedValidation script `shouldBe` Left (NotRecommended DuplicateSignatures)

        it "duplicate content in RequireAnyOf" $ do
            let script = RequireAnyOf [verKeyHash1, verKeyHash2, verKeyHash1]
            validateScript RecommendedValidation script `shouldBe` Left (NotRecommended DuplicateSignatures)

        it "duplicate content in RequireSomeOf" $ do
            let script = RequireSomeOf 1 [verKeyHash1, verKeyHash2, verKeyHash1]
            validateScript RecommendedValidation script `shouldBe` Left (NotRecommended DuplicateSignatures)

        it "duplicate in nested" $ do
            let script = RequireSomeOf 1
                    [ verKeyHash1
                    , RequireAnyOf [ verKeyHash2
                                   , RequireSomeOf 2 [verKeyHash3, verKeyHash3, verKeyHash4]
                                   ]
                    ]
            validateScript RecommendedValidation script `shouldBe` Left (NotRecommended DuplicateSignatures)

        it "redundant timelocks - too many" $ do
            let script = RequireSomeOf 1 [verKeyHash1, ActiveFromSlot 1, ActiveFromSlot 2, ActiveUntilSlot 120]
            validateScript RecommendedValidation script `shouldBe` Left (NotRecommended RedundantTimelocks)

        it "redundant timelocks - nested" $ do
            let script = RequireSomeOf 1
                    [ verKeyHash1
                    , RequireAnyOf [ verKeyHash2
                                   , RequireSomeOf 2 [verKeyHash3, verKeyHash4, ActiveFromSlot 1, ActiveFromSlot 2, ActiveUntilSlot 120, ActiveUntilSlot 125, verKeyHash1]
                                   ]
                    ]
            validateScript RecommendedValidation script `shouldBe` Left (NotRecommended RedundantTimelocks)

        it "content in RequireAllOf - 1" $ do
            let script = RequireAllOf [verKeyHash1]
            validateScript RecommendedValidation script `shouldBe` Right ()

        it "content in RequireAllOf - 2" $ do
            let script = RequireAllOf [verKeyHash1, verKeyHash2]
            validateScript RecommendedValidation script `shouldBe` Right ()

        it "nested 1" $ do
            let script = RequireSomeOf 1
                    [ verKeyHash1
                    , RequireAnyOf
                        [ verKeyHash2
                        , RequireSomeOf 1 [verKeyHash3, verKeyHash4]
                        ]
                    ]
            validateScript RecommendedValidation script `shouldBe` Right ()

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
            validateScript RecommendedValidation script `shouldBe` Right ()

        it "not uniform prefixes in script" $ do
            let script = RequireAllOf
                    [ RequireAnyOf
                      [ verKeyHash1, verKeyHash5 ]
                    ]
            validateScript RequiredValidation script `shouldBe` (Left NotUniformKeyType)

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
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` (Left NoCosigner)

        it "illegal cosigner in script template" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSignatureOf (Cosigner 4))
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` (Left UnknownCosigner)

        it "not all cosigners used in script template" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAnyOf [cosigner0, cosigner1, cosigner2] )
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` (Left UnusedCosigner)

        it "duplicated xpub in cosigners in script template" $ do
            let scriptTemplate = ScriptTemplate cosignersWrong (RequireSignatureOf (Cosigner 1))
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` (Left DuplicateXPubs)

        it "no content in RequireAnyOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAllOf [cosigner0, cosigner1, cosigner2, cosigner3, RequireAnyOf []])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript LedgerIncompatible)

        it "no content in RequireSomeOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAllOf [cosigner0, cosigner1, cosigner2, cosigner3, RequireSomeOf 1 []])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript LedgerIncompatible)

        it "too high m in RequireSomeOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 5 [cosigner0, cosigner1, cosigner2, cosigner3])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript LedgerIncompatible)

        it "m=0 in RequireSomeOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 0 [cosigner0, cosigner1, cosigner2, cosigner3])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe`Left (WrongScript $ NotRecommended MZero)

        it "wrong in nested 1" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1 [cosigner0, cosigner1, cosigner2, cosigner3, RequireAllOf [] ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript $ NotRecommended EmptyList)

        it "wrong in nested 2" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner0, cosigner1, cosigner2, cosigner3
                    , RequireAnyOf [cosigner2, RequireAllOf [] ]
                    ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript $ NotRecommended EmptyList)

        it "wrong in nested 3" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner0, cosigner1, cosigner2, cosigner3
                    , RequireAnyOf [ cosigner2
                                   , RequireSomeOf 3 [cosigner0, cosigner3]
                                   ]
                    ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript $ NotRecommended ListTooSmall)

        it "duplicate content in RequireAllOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAllOf [cosigner1, cosigner2, cosigner1,cosigner0, cosigner3 ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript $ NotRecommended DuplicateSignatures)

        it "duplicate content in RequireAnyOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAnyOf [cosigner1, cosigner2, cosigner1,cosigner0, cosigner3 ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript $ NotRecommended DuplicateSignatures)

        it "duplicate content in RequireSomeOf" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1 [cosigner1, cosigner2, cosigner1,cosigner0, cosigner3 ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript $ NotRecommended DuplicateSignatures)

        it "duplicate in nested" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner1
                    , RequireAnyOf [ cosigner2
                                   , RequireSomeOf 2 [cosigner0, cosigner0, cosigner3]
                                   ]
                    ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript $ NotRecommended DuplicateSignatures)

        it "invalid timelocks - too many" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAllOf [cosigner0, cosigner1, cosigner2 ,cosigner3, ActiveFromSlot 1, ActiveFromSlot 2, ActiveUntilSlot 25])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript $ NotRecommended RedundantTimelocks)

        it "valid timelocks" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireAllOf [cosigner0, cosigner1, cosigner2 ,cosigner3, ActiveFromSlot 11, ActiveUntilSlot 11])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Right ()

        it "valid timelocks when using all 1" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner1
                    , RequireAnyOf [ cosigner2
                                   , RequireAllOf [cosigner0, cosigner1, cosigner2 ,cosigner3, ActiveFromSlot 21, ActiveUntilSlot 25]
                                   ]
                    ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Right ()

        it "valid timelocks when using all 2" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner1
                    , RequireAnyOf [ cosigner2
                                   , RequireAllOf [cosigner0, cosigner1, cosigner2 ,cosigner3, ActiveFromSlot 21, ActiveUntilSlot 21]
                                   ]
                    ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Right ()

        it "invalid timelocks when using all" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner1
                    , RequireAnyOf [ cosigner2
                                   , RequireAllOf [cosigner0, cosigner1, cosigner2 ,cosigner3, ActiveFromSlot 25, ActiveUntilSlot 21]
                                   ]
                    ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript $ NotRecommended TimelockTrap)

        it "valid timelocks when using any" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner1
                    , RequireAnyOf [ cosigner2
                                   , RequireAnyOf [cosigner0, cosigner1, cosigner2 ,cosigner3, ActiveFromSlot 25, ActiveUntilSlot 21]
                                   ]
                    ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Right ()

        it "invalid timelocks when using any 1" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner1
                    , RequireAnyOf [ cosigner2
                                   , RequireAnyOf [cosigner0, cosigner1, cosigner2 ,cosigner3, ActiveFromSlot 21, ActiveUntilSlot 25]
                                   ]
                    ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript $ NotRecommended RedundantTimelocks)

        it "invalid timelocks when using any 2" $ do
            let scriptTemplate = ScriptTemplate cosigners' (RequireSomeOf 1
                    [ cosigner1
                    , RequireAnyOf [ cosigner2
                                   , RequireAnyOf [cosigner0, cosigner1, cosigner2 ,cosigner3, ActiveFromSlot 21, ActiveUntilSlot 21]
                                   ]
                    ])
            validateScriptTemplate RecommendedValidation scriptTemplate `shouldBe` Left (WrongScript $ NotRecommended RedundantTimelocks)

    describe "validateScriptInScriptTemplate - errors" $ do
        let cosigner0 = RequireSignatureOf (Cosigner 0)
        let cosigner1 = RequireSignatureOf (Cosigner 1)
        let cosigner2 = RequireSignatureOf (Cosigner 2)
        let cosigner3 = RequireSignatureOf (Cosigner 3)

        it "correct RequireAllOf []" $ do
            let script = ScriptTemplate Map.empty $ RequireAllOf []
            validateScriptInScriptTemplate RequiredValidation script `shouldBe` (Right ())

        it "incorrect RequireAnyOf []" $ do
            let script = ScriptTemplate Map.empty $ RequireAnyOf []
            validateScriptInScriptTemplate RequiredValidation script `shouldBe` (Left LedgerIncompatible)

        it "incorrect RequireSomeOf 1" $ do
            let script = ScriptTemplate Map.empty $ RequireSomeOf 2 [cosigner0]
            validateScriptInScriptTemplate RequiredValidation script `shouldBe` (Left LedgerIncompatible)

        it "incorrect RequireSomeOf 2" $ do
            let script = ScriptTemplate Map.empty $
                    RequireSomeOf 2 [cosigner0, cosigner0, RequireAnyOf []]
            validateScriptInScriptTemplate RequiredValidation script `shouldBe` Right ()

        it "correct RequireSomeOf" $ do
            let script = ScriptTemplate Map.empty $
                    RequireSomeOf 2 [cosigner0,  cosigner1, RequireAnyOf []]
            validateScriptInScriptTemplate RequiredValidation script `shouldBe` Right ()

        it "m=0 in RequireSomeOf is correct" $ do
            let script = ScriptTemplate Map.empty $
                    RequireSomeOf 0 [cosigner2, cosigner3]
            validateScriptInScriptTemplate RequiredValidation script  `shouldBe` Right ()

        it "timelocks are correct if timelocks are disjoint" $ do
            let script = ScriptTemplate Map.empty $
                    RequireSomeOf 2 [ActiveFromSlot 9, ActiveUntilSlot 8 ]
            validateScriptInScriptTemplate RequiredValidation script `shouldBe` Right ()

    describe "can perform roundtrip JSON serialization & deserialization - Script KeyHash" $
        it "fromJSON . toJSON === pure" $
        property (prop_jsonRoundtrip @(Script KeyHash))
    describe "can perform roundtrip JSON serialization & deserialization - Script KeyHash validated" $
        it "fromJSON . toJSON === pure" $
        property (prop_jsonRoundtripWithValidation (validateScript RequiredValidation))
    describe "can perform roundtrip JSON serialization & deserialization - Script Cosigner" $
        it "fromJSON . toJSON === pure" $
        property (prop_jsonRoundtrip @(Script Cosigner))
    describe "can perform roundtrip JSON serialization & deserialization - ScriptTemplate" $
        it "fromJSON . toJSON === pure" $
        property (prop_jsonRoundtrip @ScriptTemplate)
    describe "can perform roundtrip JSON serialization & deserialization - ScriptTemplate validated" $
        it "fromJSON . toJSON === pure" $
        property (prop_jsonRoundtripWithValidation (validateScriptTemplate RequiredValidation))

    describe "some JSON parsing error" $ do
        it "Invalid type" $ do
            let err = "Error in $.all[0].any[0]: expected Object or String, but encountered Number"
            Json.eitherDecode @(Script KeyHash) "{ \"all\": [ { \"any\": [1,2,3] } ] }"
                `shouldBe` Left err

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
            case Json.eitherDecode @(Script KeyHash) "'';[][" of
                Right _ -> expectationFailure "Parsed invalid json?"
                Left msg -> msg `shouldStartWith` err

  where
    toHexText = T.decodeUtf8 . encode EBase16
    toHexText' (ScriptHash bytes) = toHexText bytes

prop_jsonRoundtripWithValidation
    :: (Eq a, Show a, ToJSON a, FromJSON a)
    => (a -> Either err ())
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
    arbitrary = do
        payload' <- BS.pack <$> vectorOf 28 arbitrary
        flip KeyHash payload' <$> oneof [pure Payment, pure Delegation]

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
