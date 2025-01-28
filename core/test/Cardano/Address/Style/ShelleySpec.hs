{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Address.Style.ShelleySpec
    ( spec
    ) where

import Prelude

import Cardano.Address
    ( Address
    , ChainPointer (..)
    , HasNetworkDiscriminant (..)
    , bech32
    , bech32With
    , fromBech32
    , unAddress
    , unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index (..)
    , Pub
    , SoftDerivation (..)
    , XPrv
    , XPub
    , hashCredential
    , indexFromWord32
    , pubFromBytes
    , pubToBytes
    , toXPub
    , unsafeMkIndex
    , xprvPrivateKey
    , xprvToBytes
    , xpubFromBytes
    , xpubToBytes
    , xpubToPub
    )
import Cardano.Address.KeyHash
    ( GovernanceType (..), KeyHash (..), KeyRole (..), keyHashToText )
import Cardano.Address.Script
    ( Script (..), ScriptHash (..), toScriptHash )
import Cardano.Address.Style.Shelley
    ( Credential (..)
    , Role (..)
    , Shelley (..)
    , delegationAddress
    , deriveCCColdPrivateKey
    , deriveCCHotPrivateKey
    , deriveDRepPrivateKey
    , deriveDelegationPrivateKey
    , liftPub
    , liftXPub
    , mkNetworkDiscriminant
    , paymentAddress
    , pointerAddress
    , roleFromIndex
    , roleToIndex
    )
import Cardano.Mnemonic
    ( SomeMnemonic, mkSomeMnemonic )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode, fromBase16 )
import Control.Monad
    ( (<=<) )
import Crypto.Hash
    ( hashWith )
import Crypto.Hash.Algorithms
    ( SHA3_256 (SHA3_256) )
import Data.Aeson
    ( ToJSON, Value (..) )
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( rights )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe, isNothing )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import Test.Arbitrary
    ()
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.Hspec.Golden
    ( Golden (..) )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , counterexample
    , label
    , property
    , vector
    , (===)
    )
import Text.Pretty.Simple
    ( defaultOutputOptionsNoColor, pShowOpt )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TL

spec :: Spec
spec = do
    describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i) for any key" $
            property prop_publicChildKeyDerivation

    describe "Role indices" $ do
        it "Calling indexFromWord32 for invalid value fails)" $
            property prop_fromWord32Role
        it "Roundtrip index" $ property prop_roundtripIndexRole

    describe "Proper pointer addresses construction" $ do
        it "Using different numbers in DelegationPointerAddress does not fail"
            (property prop_pointerAddressConstruction)

    describe "Text Encoding Roundtrips" $ do
        prop "bech32 . fromBech32 - Shelley - payment address" $
            prop_roundtripTextEncoding bech32 fromBech32

        prop "bech32 . fromBech32 - Shelley - delegation address" $
            prop_roundtripTextEncodingDelegation bech32 fromBech32

        prop "bech32 . fromBech32 - Shelley - pointer address" $
            prop_roundtripTextEncodingPointer bech32 fromBech32

    describe "Golden tests" $ do
        goldenTestPointerAddress GoldenTestPointerAddress
            {  verKey = "1a2a3a4a5a6a7a8a"
            ,  stakePtr = ChainPointer 128 2 3
            ,  netTag = 0
            ,  expectedAddr =
                    "408a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d481000203"
            }
        goldenTestPointerAddress GoldenTestPointerAddress
            {  verKey = "1a2a3a4a5a6a7a8a"
            ,  stakePtr = ChainPointer 128 2 3
            ,  netTag = 1
            ,  expectedAddr =
                    "418a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d481000203"
            }
        goldenTestEnterpriseAddress GoldenTestEnterpriseAddress
            {  verKey = "1a2a3a4a5a6a7a8a"
            ,  netTag = 0
            ,  expectedAddr =
                    "608a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4"
            }
        goldenTestEnterpriseAddress GoldenTestEnterpriseAddress
            {  verKey = "1a2a3a4a5a6a7a8a"
            ,  netTag = 1
            ,  expectedAddr =
                    "618a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4"
            }
        goldenTestEnterpriseAddressKeyHash GoldenTestEnterpriseAddress
            {  verKey = "1a2a3a4a5a6a7a8a"
            ,  netTag = 0
            ,  expectedAddr =
                    "608a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4"
            }
        goldenTestEnterpriseAddressKeyHash GoldenTestEnterpriseAddress
            {  verKey = "1a2a3a4a5a6a7a8a"
            ,  netTag = 1
            ,  expectedAddr =
                    "618a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4"
            }
        goldenTestBaseAddressPayFromXPub GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 0
            ,  expectedAddr =
                    "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressPayFromXPub GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 1
            ,  expectedAddr =
                    "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressPayFromPub GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 0
            ,  expectedAddr =
                    "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressPayFromPub GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 1
            ,  expectedAddr =
                    "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressPayFromKeyHash GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 0
            ,  expectedAddr =
                    "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressPayFromKeyHash GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 1
            ,  expectedAddr =
                    "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressStakeFromKeyHash GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 0
            ,  expectedAddr =
                    "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressStakeFromKeyHash GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 1
            ,  expectedAddr =
                    "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressStakeFromPub GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 0
            ,  expectedAddr =
                    "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressStakeFromPub GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 1
            ,  expectedAddr =
                    "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressBothFromKeyHash GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 0
            ,  expectedAddr =
                    "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressBothFromKeyHash GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 1
            ,  expectedAddr =
                    "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressBothFromPub GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 0
            ,  expectedAddr =
                    "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddressBothFromPub GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 1
            ,  expectedAddr =
                    "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestGovernance GoldenTestGovernance
            {  mnemonic = [ "test", "walk", "nut", "penalty", "hip", "pave", "soap",
                            "entry", "language", "right", "filter", "choice" ]
            ,  accountIx = 0x80000000
            ,  expectedKeysHashes = KeysHashes
                   { drepXsk = "drep_xsk14rjh4rs2dzm6k5xxe5f73cypzuv02pknfl9xwnsjws8a7ulp530xztarpdlyh05csw2cmnekths7dstq0se3wtza84m4fueffezsjfgassgs9xtfzgehrn0fn7c82uc0rkj06s0w0t80hflzz8cwyry3eg9066uj"
                   , drepXskHex = "a8e57a8e0a68b7ab50c6cd13e8e0811718f506d34fca674e12740fdf73e1a45e612fa30b7e4bbe9883958dcf365de1e6c1607c33172c5d3d7754f3294e4509251d8411029969123371cde99fb075730f1da4fd41ee7acefba7e211f0e20c91ca"
                   , drepSk = "drep_sk14rjh4rs2dzm6k5xxe5f73cypzuv02pknfl9xwnsjws8a7ulp530xztarpdlyh05csw2cmnekths7dstq0se3wtza84m4fueffezsjfglsqmad"
                   , drepSkHex = "a8e57a8e0a68b7ab50c6cd13e8e0811718f506d34fca674e12740fdf73e1a45e612fa30b7e4bbe9883958dcf365de1e6c1607c33172c5d3d7754f3294e450925"
                   , drepXvk = "drep_xvk17axh4sc9zwkpsft3tlgpjemfwc0u5mnld80r85zw7zdqcst6w543mpq3q2vkjy3nw8x7n8asw4es78dyl4q7u7kwlwn7yy0sugxfrjs6z25qe"
                   , drepXvkHex = "f74d7ac30513ac1825715fd0196769761fca6e7f69de33d04ef09a0c417a752b1d8411029969123371cde99fb075730f1da4fd41ee7acefba7e211f0e20c91ca"
                   , drepVk = "drep_vk17axh4sc9zwkpsft3tlgpjemfwc0u5mnld80r85zw7zdqcst6w54sdv4a4e"
                   , drepVkHex = "f74d7ac30513ac1825715fd0196769761fca6e7f69de33d04ef09a0c417a752b"
                   , drep = "drep1y2jmg4g450lced7q9n34rq6d5vjwkm0ugx6h0894u6ur92s9txn3a"
                   , drepHex = "a5b45515a3ff8cb7c02ce351834da324eb6dfc41b5779cb5e6b832aa"
                   , drepScript1 = "drep16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v22aculf"
                   , drepScript1Hex = "d0657126dbf0c135a7224d91ca068f5bf769af6d1f1df0bce5170ec5"
                   , drepScript2 = "drep14edv7pg3y4wkglyykvvy5t2j906ld3dhdwvf7jda8qaa670f745"
                   , drepScript2Hex = "ae5acf0511255d647c84b3184a2d522bf5f6c5b76b989f49bd383bdd"
                   , ccColdXsk = "cc_cold_xsk1dp84kjq9qa647wr70e2yedzt8e27kwugh8mfw675re0hgm8p530z3d9230cjjzyyzlq04hn94x9q2m9um2tvp2y8fn7tau9l2wfj5ykxqxtgua0lxpf0lfn44md2afyl7dktyvpkmug9u28p6v452flxeuca0v7w"
                   , ccColdXskHex = "684f5b480507755f387e7e544cb44b3e55eb3b88b9f6976bd41e5f746ce1a45e28b4aa8bf129088417c0fade65a98a056cbcda96c0a8874cfcbef0bf53932a12c601968e75ff3052ffa675aedaaea49ff36cb23036df105e28e1d32b4527e6cf"
                   , ccColdSk = "cc_cold_sk1dp84kjq9qa647wr70e2yedzt8e27kwugh8mfw675re0hgm8p530z3d9230cjjzyyzlq04hn94x9q2m9um2tvp2y8fn7tau9l2wfj5yslmdl88"
                   , ccColdSkHex = "684f5b480507755f387e7e544cb44b3e55eb3b88b9f6976bd41e5f746ce1a45e28b4aa8bf129088417c0fade65a98a056cbcda96c0a8874cfcbef0bf53932a12"
                   , ccColdXvk = "cc_cold_xvk149up407pvp9p36lldlp4qckqqzn6vm7u5yerwy8d8rqalse3t04vvqvk3e6l7vzjl7n8ttk646jflumvkgcrdhcstc5wr5etg5n7dnc8nqv5d"
                   , ccColdXvkHex = "a9781abfc1604a18ebff6fc35062c000a7a66fdca1323710ed38c1dfc3315beac601968e75ff3052ffa675aedaaea49ff36cb23036df105e28e1d32b4527e6cf"
                   , ccColdVk = "cc_cold_vk149up407pvp9p36lldlp4qckqqzn6vm7u5yerwy8d8rqalse3t04q7qsvwl"
                   , ccColdVkHex = "a9781abfc1604a18ebff6fc35062c000a7a66fdca1323710ed38c1dfc3315bea"
                   , ccCold = "cc_cold1ztl0h9vka4ns45kfj7xh3ljwkd46yn9m5znzlfxd6rpdeagw6p59q"
                   , ccColdHex = "fefb9596ed670ad2c9978d78fe4eb36ba24cbba0a62fa4cdd0c2dcf5"
                   , ccColdScript1 = "cc_cold14ehj5f64f40xju0086fnunctulkh46mq7munm7upe4hpc434cg3"
                   , ccColdScript1Hex = "ae6f2a27554d5e6971ef3e933e4f0be7ed7aeb60f6f93dfb81cd6e1c"
                   , ccColdScript2 = "cc_cold1zxwzpnk0ah7m5ptjjtmkhvgs4736k3e0ns66shd0fy33vkd39j0"
                   , ccColdScript2Hex = "119c20cecfedfdba057292f76bb110afa3ab472f9c35a85daf492316"
                   , ccHotXsk = "cc_hot_xsk1mpt30ys7v2ykqms4c83wuednh4hvy3lr27yfhgtp0rhdka8p5300j4d2z77sq2t3kp082qzgkanwkm05mp2u2nwja3ad3pgw9l34a0j5sl5yd6d8pze8dqwksd069kkfdqggk0yytcmet96fre45w64qkgyxl0dt"
                   , ccHotXskHex = "d85717921e6289606e15c1e2ee65b3bd6ec247e357889ba16178eedb74e1a45ef955aa17bd002971b05e750048b766eb6df4d855c54dd2ec7ad8850e2fe35ebe5487e846e9a708b27681d6835fa2dac968108b3c845e379597491e6b476aa0b2"
                   , ccHotSk = "cc_hot_sk1mpt30ys7v2ykqms4c83wuednh4hvy3lr27yfhgtp0rhdka8p5300j4d2z77sq2t3kp082qzgkanwkm05mp2u2nwja3ad3pgw9l34a0sdh7u7e"
                   , ccHotSkHex = "d85717921e6289606e15c1e2ee65b3bd6ec247e357889ba16178eedb74e1a45ef955aa17bd002971b05e750048b766eb6df4d855c54dd2ec7ad8850e2fe35ebe"
                   , ccHotXvk = "cc_hot_xvk10y48lq72hypxraew74lwjjn9e2dscuwphckglh2nrrpkgweqk5h4fplggm56wz9jw6qadq6l5tdvj6qs3v7ggh3hjkt5j8ntga42pvs5rvh0a"
                   , ccHotXvkHex = "792a7f83cab90261f72ef57ee94a65ca9b0c71c1be2c8fdd5318c3643b20b52f5487e846e9a708b27681d6835fa2dac968108b3c845e379597491e6b476aa0b2"
                   , ccHotVk = "cc_hot_vk10y48lq72hypxraew74lwjjn9e2dscuwphckglh2nrrpkgweqk5hschnzv5"
                   , ccHotVkHex = "792a7f83cab90261f72ef57ee94a65ca9b0c71c1be2c8fdd5318c3643b20b52f"
                   , ccHot = "cc_hot1qtmd98q0w9jdxasse0m8kyn2nya7kf9qwmgx20cl5p543rcdtr4dz"
                   , ccHotHex = "f6d29c0f7164d37610cbf67b126a993beb24a076d0653f1fa069588f"
                   , ccHotScript1 = "cc_hot16fayy2wf9myfvxmtl5e2suuqmnhy5zx80vxkezen7xqws04z0n8"
                   , ccHotScript1Hex = "d27a4229c92ec8961b6bfd32a87380dcee4a08c77b0d6c8b33f180e8"
                   , ccHotScript2 = "cc_hot1vts8nrrsxmlntp3v7sh5u7k6qmmlkkmyv5uspq4xjxlpgr6svrf"
                   , ccHotScript2Hex = "62e0798c7036ff35862cf42f4e7ada06f7fb5b6465390082a691be14"
                   }
            }
        goldenTestGovernance GoldenTestGovernance
            {  mnemonic = [ "test", "walk", "nut", "penalty", "hip", "pave", "soap",
                            "entry", "language", "right", "filter", "choice" ]
            ,  accountIx = 0x80000100
            ,  expectedKeysHashes = KeysHashes
                   { drepXsk = "drep_xsk1zracgd4mqt32f5cj0ps0wudf78u6lumz7gprgm3j8zec5ahp530weq4z9ayj6jzj33lpj86jkk2gnt0ns0d5sywteexxehvva7gugz99ydmpemzpsfnj49vjvw88q9a2s2hxc9ggxal5q6xsqz5vaat2xqsha72w"
                   , drepXskHex = "10fb8436bb02e2a4d3127860f771a9f1f9aff362f202346e3238b38a76e1a45eec82a22f492d48528c7e191f52b59489adf383db4811cbce4c6cdd8cef91c408a523761cec4182672a9592638e7017aa82ae6c1508377f4068d000a8cef56a30"
                   , drepSk = "drep_sk1zracgd4mqt32f5cj0ps0wudf78u6lumz7gprgm3j8zec5ahp530weq4z9ayj6jzj33lpj86jkk2gnt0ns0d5sywteexxehvva7gugzqjur0zk"
                   , drepSkHex = "10fb8436bb02e2a4d3127860f771a9f1f9aff362f202346e3238b38a76e1a45eec82a22f492d48528c7e191f52b59489adf383db4811cbce4c6cdd8cef91c408"
                   , drepXvk = "drep_xvk1wq6ylcpjnwavhveey855tkhdrqdav6yfxvltw0emky9d3erxn9m22gmkrnkyrqn8922eycuwwqt64q4wds2ssdmlgp5dqq9gem6k5vq23ph3c"
                   , drepXvkHex = "70344fe0329bbacbb33921e945daed181bd66889333eb73f3bb10ad8e4669976a523761cec4182672a9592638e7017aa82ae6c1508377f4068d000a8cef56a30"
                   , drepVk = "drep_vk1wq6ylcpjnwavhveey855tkhdrqdav6yfxvltw0emky9d3erxn9mqdrlerg"
                   , drepVkHex = "70344fe0329bbacbb33921e945daed181bd66889333eb73f3bb10ad8e4669976"
                   , drep = "drep1yg0dx99005ll3lxnyrrnadv9ynthfj3cwvlwqr4u4qdavwshx0yl0"
                   , drepHex = "1ed314af7d3ff8fcd320c73eb58524d774ca38733ee00ebca81bd63a"
                   , drepScript1 = "drep18cgl8kdnjculhww4n3h0a3ahc85ahjcsg53u0f93jnz9cqnvdpd"
                   , drepScript1Hex = "3e11f3d9b39639fbb9d59c6efec7b7c1e9dbcb104523c7a4b194c45c"
                   , drepScript2 = "drep1hwj9yuvzxc623w5lmwvp44md7qkdywz2fcd583qmyu62jrsw2xy"
                   , drepScript2Hex = "bba45271823634a8ba9fdb981ad76df02cd2384a4e1b43c41b2734a9"
                   , ccColdXsk = "cc_cold_xsk1dppxrjspxrjj5e5xrmh6yaw6w30arsl5lqcsp09ynyzwwulp530q4tlvug79xx6ja3u32fu9jyy84p6erjmza6twrackm9kfsdpc3ap7uxpempqjftx74qwxnmn7d6pg8pl9zpnc0rese26pfmzl9cmtgg8xsxvu"
                   , ccColdXskHex = "684261ca0130e52a66861eefa275da745fd1c3f4f83100bca49904e773e1a45e0aafece23c531b52ec7915278591087a87591cb62ee96e1f716d96c9834388f43ee1839d84124acdea81c69ee7e6e828387e51067878f30cab414ec5f2e36b42"
                   , ccColdSk = "cc_cold_sk1dppxrjspxrjj5e5xrmh6yaw6w30arsl5lqcsp09ynyzwwulp530q4tlvug79xx6ja3u32fu9jyy84p6erjmza6twrackm9kfsdpc3aqr79jja"
                   , ccColdSkHex = "684261ca0130e52a66861eefa275da745fd1c3f4f83100bca49904e773e1a45e0aafece23c531b52ec7915278591087a87591cb62ee96e1f716d96c9834388f4"
                   , ccColdXvk = "cc_cold_xvk1e2mquwugpwnykfftjs4mv3w4uk80f4hjgd2zls5vusz3zuqhr7gnacvrnkzpyjkda2qud8h8um5zswr72yr8s78npj45znk97t3kkssryhkyv"
                   , ccColdXvkHex = "cab60e3b880ba64b252b942bb645d5e58ef4d6f243542fc28ce4051170171f913ee1839d84124acdea81c69ee7e6e828387e51067878f30cab414ec5f2e36b42"
                   , ccColdVk = "cc_cold_vk1e2mquwugpwnykfftjs4mv3w4uk80f4hjgd2zls5vusz3zuqhr7gs3qg4hr"
                   , ccColdVkHex = "cab60e3b880ba64b252b942bb645d5e58ef4d6f243542fc28ce4051170171f91"
                   , ccCold = "cc_cold1zt5nwd86uuvwjxalghyxlrxcreledplxellyeygd6xjvxcqy66a7t"
                   , ccColdHex = "e93734fae718e91bbf45c86f8cd81e7f9687e6cffe4c910dd1a4c360"
                   , ccColdScript1 = "cc_cold1prtcxdlu75dz48lf8hh86gt8ng7z39yvmyqcg92sgze7gpt2gga"
                   , ccColdScript1Hex = "08d78337fcf51a2a9fe93dee7d21679a3c28948cd90184155040b3e4"
                   , ccColdScript2 = "cc_cold1969h0m92nuqrj7x74pj3tnhxh97lfhl4y2vwvqvc6kecwkq6xe5"
                   , ccColdScript2Hex = "2e8b77ecaa9f003978dea86515cee6b97df4dff52298e60198d5b387"
                   , ccHotXsk = "cc_hot_xsk15pt89wppyhr9eqgm5nnu7tna3dfmqxa2u45e4g7krzp9u78p530pez36k8k9n0gw08hn6drxlwxxsgc4jsejv6hvcnkd7gd3zxhstpe3vzde6e98zql6n2cmekklm63dydnt80szdr0h768dexeklrfspc5lznuz"
                   , ccHotXskHex = "a05672b82125c65c811ba4e7cf2e7d8b53b01baae5699aa3d618825e78e1a45e1c8a3ab1ec59bd0e79ef3d3466fb8c6823159433266aecc4ecdf21b111af058731609b9d64a7103fa9ab1bcdadfdea2d2366b3be0268df7f68edc9b36f8d300e"
                   , ccHotSk = "cc_hot_sk15pt89wppyhr9eqgm5nnu7tna3dfmqxa2u45e4g7krzp9u78p530pez36k8k9n0gw08hn6drxlwxxsgc4jsejv6hvcnkd7gd3zxhstpc7gujxf"
                   , ccHotSkHex = "a05672b82125c65c811ba4e7cf2e7d8b53b01baae5699aa3d618825e78e1a45e1c8a3ab1ec59bd0e79ef3d3466fb8c6823159433266aecc4ecdf21b111af0587"
                   , ccHotXvk = "cc_hot_xvk10qawpxlz7eytt9yr4xlwtjkw345v0ehzsxdlkks6qralyp975phrzcymn4j2wypl4x43hnddlh4z6gmxkwlqy6xl0a5wmjdnd7xnqrsvak8ry"
                   , ccHotXvkHex = "783ae09be2f648b59483a9bee5cace8d68c7e6e2819bfb5a1a00fbf204bea06e31609b9d64a7103fa9ab1bcdadfdea2d2366b3be0268df7f68edc9b36f8d300e"
                   , ccHotVk = "cc_hot_vk10qawpxlz7eytt9yr4xlwtjkw345v0ehzsxdlkks6qralyp975phqx538xn"
                   , ccHotVkHex = "783ae09be2f648b59483a9bee5cace8d68c7e6e2819bfb5a1a00fbf204bea06e"
                   , ccHot = "cc_hot1qtgaf67mr95fa90qj7gehkr3y3q73x6pasmdu3algq6ylpgfamj02"
                   , ccHotHex = "d1d4ebdb19689e95e097919bd8712441e89b41ec36de47bf40344f85"
                   , ccHotScript1 = "cc_hot1hheftszv4jw83f5megrvhrevl7lwwmtnjav7srkqngr92348jvr"
                   , ccHotScript1Hex = "bdf295c04cac9c78a69bca06cb8f2cffbee76d739759e80ec09a0655"
                   , ccHotScript2 = "cc_hot1dg9jdwlsxzakctywv2cw7a7ggj2dwu0gz5tueu2rf40zv05atgs"
                   , ccHotScript2Hex = "6a0b26bbf030bb6c2c8e62b0ef77c84494d771e81517ccf1434d5e26"
                   }
            }
        goldenTestGovernance GoldenTestGovernance
            {  mnemonic = [ "excess", "behave", "track", "soul", "table", "wear",
                            "ocean", "cash", "stay", "nature", "item", "turtle",
                            "palm", "soccer", "lunch", "horror", "start", "stumble",
                            "month", "panic", "right", "must", "lock", "dress" ]
            ,  accountIx = 0x80000000
            ,  expectedKeysHashes = KeysHashes
                   { drepXsk = "drep_xsk17pwn6d7pu0d6sfzysyk5taux99f5tdqsct7zzthgljyd5zs33azej0tm5ny7ksunthqu84tqg832md6vs3hm392agwx3auhvyjtzxr2l6c0dj47k6zedl4kgugneu04j64fc5uueayydmufdrdaled9k4qllaka6"
                   , drepXskHex = "f05d3d37c1e3dba82444812d45f786295345b410c2fc212ee8fc88da0a118f45993d7ba4c9eb43935dc1c3d56041e2adb74c846fb8955d438d1ef2ec2496230d5fd61ed957d6d0b2dfd6c8e2279e3eb2d5538a7399e908ddf12d1b7bfcb4b6a8"
                   , drepSk = "drep_sk17pwn6d7pu0d6sfzysyk5taux99f5tdqsct7zzthgljyd5zs33azej0tm5ny7ksunthqu84tqg832md6vs3hm392agwx3auhvyjtzxrgwyexuy"
                   , drepSkHex = "f05d3d37c1e3dba82444812d45f786295345b410c2fc212ee8fc88da0a118f45993d7ba4c9eb43935dc1c3d56041e2adb74c846fb8955d438d1ef2ec2496230d"
                   , drepXvk = "drep_xvk15j30gk0uex88lc9vh6sfda93lv6zede65mzp7ck56m9pgeqhnht9l4s7m9tad59jmltv3c38nclt942n3feen6ggmhcj6xmmlj6td2qu4ce82"
                   , drepXvkHex = "a4a2f459fcc98e7fe0acbea096f4b1fb342cb73aa6c41f62d4d6ca1464179dd65fd61ed957d6d0b2dfd6c8e2279e3eb2d5538a7399e908ddf12d1b7bfcb4b6a8"
                   , drepVk = "drep_vk15j30gk0uex88lc9vh6sfda93lv6zede65mzp7ck56m9pgeqhnhtqvs6j8t"
                   , drepVkHex = "a4a2f459fcc98e7fe0acbea096f4b1fb342cb73aa6c41f62d4d6ca1464179dd6"
                   , drep = "drep1yge7tpltrazw286rqlhw6lk7vxgq30zdrsevrqye6cm8x2gf38vlv"
                   , drepHex = "33e587eb1f44e51f4307eeed7ede619008bc4d1c32c18099d6367329"
                   , drepScript1 = "drep17fql6ztxyk63taryk2e4mh47jw3wdchv9e7u4jxg4edrxg8enef"
                   , drepScript1Hex = "f241fd096625b515f464b2b35ddebe93a2e6e2ec2e7dcac8c8ae5a33"
                   , drepScript2 = "drep10qp23w0gppuvc7chc3g7saudlmhj9jmm9ssrrzzm3qwksrn4cul"
                   , drepScript2Hex = "7802a8b9e80878cc7b17c451e8778dfeef22cb7b2c2031885b881d68"
                   , ccColdXsk = "cc_cold_xsk1hqtevrzlhtcglwvt5pmgct8ssqx37vjjf3wuydpd6flyqrg33azacap5w5mclacmuycx3xgrtstxgrpzcncf6l840t0klmywc69ryd9zf95taaaseka98yakuj2048slnuekw22qm58majt8alhs438eecehquu0"
                   , ccColdXskHex = "b817960c5fbaf08fb98ba0768c2cf0800d1f32524c5dc2342dd27e400d118f45dc743475378ff71be1306899035c16640c22c4f09d7cf57adf6fec8ec68a3234a24968bef7b0cdba5393b6e494fa9e1f9f33672940dd0fbec967efef0ac4f9ce"
                   , ccColdSk = "cc_cold_sk1hqtevrzlhtcglwvt5pmgct8ssqx37vjjf3wuydpd6flyqrg33azacap5w5mclacmuycx3xgrtstxgrpzcncf6l840t0klmywc69rydqvncuyc"
                   , ccColdSkHex = "b817960c5fbaf08fb98ba0768c2cf0800d1f32524c5dc2342dd27e400d118f45dc743475378ff71be1306899035c16640c22c4f09d7cf57adf6fec8ec68a3234"
                   , ccColdXvk = "cc_cold_xvk13wc4cvvr266t4rxm9wyel4deeqxyylvjzjdk5w74lva2xm0dhxt6yjtghmmmpnd62wfmdey5l20pl8envu55phg0hmyk0ml0ptz0nns9cqjlk"
                   , ccColdXvkHex = "8bb15c318356b4ba8cdb2b899fd5b9c80c427d92149b6a3bd5fb3aa36dedb997a24968bef7b0cdba5393b6e494fa9e1f9f33672940dd0fbec967efef0ac4f9ce"
                   , ccColdVk = "cc_cold_vk13wc4cvvr266t4rxm9wyel4deeqxyylvjzjdk5w74lva2xm0dhxtsfpa2qu"
                   , ccColdVkHex = "8bb15c318356b4ba8cdb2b899fd5b9c80c427d92149b6a3bd5fb3aa36dedb997"
                   , ccCold = "cc_cold1ztc2kq7xa0vdrdz3tg7umg724srnw6yac0zschqdl0rerushdm3cm"
                   , ccColdHex = "f0ab03c6ebd8d1b4515a3dcda3caac0737689dc3c50c5c0dfbc791f2"
                   , ccColdScript1 = "cc_cold15z7ynn7fuqu55hh850962vrrg7tcdncl8spnjtrxjjm06lpsfgc"
                   , ccColdScript1Hex = "a0bc49cfc9e0394a5ee7a3cba53063479786cf1f3c03392c6694b6fd"
                   , ccColdScript2 = "cc_cold1ahw3qh3ledhxp0frga9aawfkxpu0qstte9nmem0phqqegzf9lp3"
                   , ccColdScript2Hex = "eddd105e3fcb6e60bd23474bdeb9363078f0416bc967bcede1b80194"
                   , ccHotXsk = "cc_hot_xsk1wzamzchtj7m79mjfpg3c02m534ugej5ac0p3s2sresr7vys33azktmjva6flctprqu6m4k4w459x9qkfsz2ahgy5ganjn23djhhkg5e5eyhu7fjxl6tpxtmzh7e2ftuj4qgmawsmcl7sqesn8e0pmh97zs3c3fqj"
                   , ccHotXskHex = "70bbb162eb97b7e2ee490a2387ab748d788cca9dc3c3182a03cc07e612118f4565ee4cee93fc2c230735badaaead0a6282c98095dba094476729aa2d95ef645334c92fcf2646fe96132f62bfb2a4af92a811beba1bc7fd0066133e5e1ddcbe14"
                   , ccHotSk = "cc_hot_sk1wzamzchtj7m79mjfpg3c02m534ugej5ac0p3s2sresr7vys33azktmjva6flctprqu6m4k4w459x9qkfsz2ahgy5ganjn23djhhkg5cmwegml"
                   , ccHotSkHex = "70bbb162eb97b7e2ee490a2387ab748d788cca9dc3c3182a03cc07e612118f4565ee4cee93fc2c230735badaaead0a6282c98095dba094476729aa2d95ef6453"
                   , ccHotXvk = "cc_hot_xvk1tazd6lvnf2c9j8m58h6xy56uuyhkee526jgxj2ylaextl0xamd4nfjf0eunydl5kzvhk90aj5jhe92q3h6aph3laqpnpx0j7rhwtu9qe7dhsc"
                   , ccHotXvkHex = "5f44dd7d934ab0591f743df462535ce12f6ce68ad49069289fee4cbfbcdddb6b34c92fcf2646fe96132f62bfb2a4af92a811beba1bc7fd0066133e5e1ddcbe14"
                   , ccHotVk = "cc_hot_vk1tazd6lvnf2c9j8m58h6xy56uuyhkee526jgxj2ylaextl0xamd4swmuygc"
                   , ccHotVkHex = "5f44dd7d934ab0591f743df462535ce12f6ce68ad49069289fee4cbfbcdddb6b"
                   , ccHot = "cc_hot1qtp2wn5mef3ypk2872d7kl0djczfwsqkmgk53ca8cdjye3qehdsy0"
                   , ccHotHex = "c2a74e9bca6240d947f29beb7ded9604974016da2d48e3a7c3644cc4"
                   , ccHotScript1 = "cc_hot1tmwlec0twwvl29h6pgvew5mf4recsxtktev9g07xm37fvvupdmd"
                   , ccHotScript1Hex = "5eddfce1eb7399f516fa0a19975369a8f38819765e58543fc6dc7c96"
                   , ccHotScript2 = "cc_hot1c77thg5lrahy0he4q6glsk8vgsp45gt75k3pq09d02u8gvktfzw"
                   , ccHotScript2Hex = "c7bcbba29f1f6e47df350691f858ec44035a217ea5a2103cad7ab874"
                   }
            }
        goldenTestGovernance GoldenTestGovernance
            {  mnemonic = [ "excess", "behave", "track", "soul", "table", "wear",
                            "ocean", "cash", "stay", "nature", "item", "turtle",
                            "palm", "soccer", "lunch", "horror", "start", "stumble",
                            "month", "panic", "right", "must", "lock", "dress" ]
            ,  accountIx = 0x80000100
            ,  expectedKeysHashes = KeysHashes
                   { drepXsk = "drep_xsk14z6a7nd2q5r03s4gxsrujc59sg757vqqcwxeuc5s874c2rq33az37lwkxpxvh5s4a94sncxp6y7m73pxsuknt7gvethhue5jk5vc5n2hrg95mynhw7mtrshxr5mpku4v8x6lpm05nznrqej70u0fllgfkusexdkv"
                   , drepXskHex = "a8b5df4daa0506f8c2a83407c96285823d4f3000c38d9e62903fab850c118f451f7dd6304ccbd215e96b09e0c1d13dbf4426872d35f90ccaef7e6692b5198a4d571a0b4d927777b6b1c2e61d361b72ac39b5f0edf498a630665e7f1e9ffd09b7"
                   , drepSk = "drep_sk14z6a7nd2q5r03s4gxsrujc59sg757vqqcwxeuc5s874c2rq33az37lwkxpxvh5s4a94sncxp6y7m73pxsuknt7gvethhue5jk5vc5ngl9zhrx"
                   , drepSkHex = "a8b5df4daa0506f8c2a83407c96285823d4f3000c38d9e62903fab850c118f451f7dd6304ccbd215e96b09e0c1d13dbf4426872d35f90ccaef7e6692b5198a4d"
                   , drepXvk = "drep_xvk14dwjrplj73qeggdsg4lh4j9tp495asyq9t6augwaue8kqvjg5wq4wxstfkf8waakk8pwv8fkrde2cwd47rklfx9xxpn9ulc7nl7sndcvdjh2m"
                   , drepXvkHex = "ab5d2187f2f4419421b0457f7ac8ab0d4b4ec0802af5de21dde64f603248a381571a0b4d927777b6b1c2e61d361b72ac39b5f0edf498a630665e7f1e9ffd09b7"
                   , drepVk = "drep_vk14dwjrplj73qeggdsg4lh4j9tp495asyq9t6augwaue8kqvjg5wqskrq5yn"
                   , drepVkHex = "ab5d2187f2f4419421b0457f7ac8ab0d4b4ec0802af5de21dde64f603248a381"
                   , drep = "drep1ytq6xshsm7uzhy729e45q6avkpyq9a74d2va3726sz5td3gqrgcll"
                   , drepHex = "c1a342f0dfb82b93ca2e6b406bacb04802f7d56a99d8f95a80a8b6c5"
                   , drepScript1 = "drep1ckr4x9293myuyz5379wndh4ag00c787htnzwzxxmpfnfzaqtayp"
                   , drepScript1Hex = "c5875315458ec9c20a91f15d36debd43df8f1fd75cc4e118db0a6691"
                   , drepScript2 = "drep1wgly5zd539aam7yxr7trxy48dhupswmwusutm4q40dwkc07n3y8"
                   , drepScript2Hex = "723e4a09b4897bddf8861f963312a76df8183b6ee438bdd4157b5d6c"
                   , ccColdXsk = "cc_cold_xsk1hqe5kcsq59mx4t9nxrctmth0ppz9gda0gnppyll3h9rxcyq33az4uy3u6qhzuhjsstzca9awgsx27j07hxhrkrk6487nvywp0ag669m4v6lj3knq7e6pxaujy98akn5exhgk44ftruepkte0hdm74dd8zceqnk2h"
                   , ccColdXskHex = "b8334b6200a1766aacb330f0bdaeef08445437af44c2127ff1b9466c10118f455e123cd02e2e5e5082c58e97ae440caf49feb9ae3b0edaa9fd3611c17f51ad177566bf28da60f674137792214fdb4e9935d16ad52b1f321b2f2fbb77eab5a716"
                   , ccColdSk = "cc_cold_sk1hqe5kcsq59mx4t9nxrctmth0ppz9gda0gnppyll3h9rxcyq33az4uy3u6qhzuhjsstzca9awgsx27j07hxhrkrk6487nvywp0ag669c5qtm3p"
                   , ccColdSkHex = "b8334b6200a1766aacb330f0bdaeef08445437af44c2127ff1b9466c10118f455e123cd02e2e5e5082c58e97ae440caf49feb9ae3b0edaa9fd3611c17f51ad17"
                   , ccColdXvk = "cc_cold_xvk1lmqejccjpxsd9cl4uavxj0jryjlfk5r8wemr0d8saal49lttp2482e4l9rdxpan5zdmeyg20md8fjdw3dt2jk8ejrvhjlwmha266w9syf55nr"
                   , ccColdXvkHex = "fec199631209a0d2e3f5e758693e4324be9b5067767637b4f0ef7f52fd6b0aaa7566bf28da60f674137792214fdb4e9935d16ad52b1f321b2f2fbb77eab5a716"
                   , ccColdVk = "cc_cold_vk1lmqejccjpxsd9cl4uavxj0jryjlfk5r8wemr0d8saal49lttp24q6lw08l"
                   , ccColdVkHex = "fec199631209a0d2e3f5e758693e4324be9b5067767637b4f0ef7f52fd6b0aaa"
                   , ccCold = "cc_cold1zfxtx2h8qhanhw3u4jt5ydtgsry39gm2ff7v5ax5j4k87sgmkas3l"
                   , ccColdHex = "4cb32ae705fb3bba3cac9742356880c912a36a4a7cca74d4956c7f41"
                   , ccColdScript1 = "cc_cold1qlk7rgkd5n6ga8enwk08vwtmlklhzfnmjtjlzlwed62tulgk75f"
                   , ccColdScript1Hex = "07ede1a2cda4f48e9f33759e76397bfdbf71267b92e5f17dd96e94be"
                   , ccColdScript2 = "cc_cold1a4qmd5d3dqppxtq5wcuuaa3xfe868vyn46afvktz5ucxzauy6tg"
                   , ccColdScript2Hex = "ed41b6d1b16802132c147639cef6264e4fa3b093aeba965962a73061"
                   , ccHotXsk = "cc_hot_xsk14rzh5lvtdhvum6vjfvkwp73mz9gl426cj04xfavnjgmdxrq33azugz0k9sekf2eg70lr34rg5aclr54v30za77xn945kncdm0le6lutxlr5ar355u5awqt2hkmdurv4qv64cmpg39zq2ahjxqken8vk62qunx4hl"
                   , ccHotXskHex = "a8c57a7d8b6dd9cde9924b2ce0fa3b1151faab5893ea64f5939236d30c118f45c409f62c3364ab28f3fe38d468a771f1d2ac8bc5df78d32d6969e1bb7ff3aff166f8e9d1c694e53ae02d57b6dbc1b2a066ab8d85112880aede4605b333b2da50"
                   , ccHotSk = "cc_hot_sk14rzh5lvtdhvum6vjfvkwp73mz9gl426cj04xfavnjgmdxrq33azugz0k9sekf2eg70lr34rg5aclr54v30za77xn945kncdm0le6lugud8v57"
                   , ccHotSkHex = "a8c57a7d8b6dd9cde9924b2ce0fa3b1151faab5893ea64f5939236d30c118f45c409f62c3364ab28f3fe38d468a771f1d2ac8bc5df78d32d6969e1bb7ff3aff1"
                   , ccHotXvk = "cc_hot_xvk1g2925ntunmthw66sr8t7v3qe7fls4575wput3936cguzk7m6w4fkd78f68rffef6uqk40dkmcxe2qe4t3kz3z2yq4m0yvpdnxwed55q798msd"
                   , ccHotXvkHex = "428aaa4d7c9ed7776b5019d7e64419f27f0ad3d47078b8963ac2382b7b7a755366f8e9d1c694e53ae02d57b6dbc1b2a066ab8d85112880aede4605b333b2da50"
                   , ccHotVk = "cc_hot_vk1g2925ntunmthw66sr8t7v3qe7fls4575wput3936cguzk7m6w4fs0zjxf8"
                   , ccHotVkHex = "428aaa4d7c9ed7776b5019d7e64419f27f0ad3d47078b8963ac2382b7b7a7553"
                   , ccHot = "cc_hot1q257k3xs4gww24vm0s3zwzkz850xr0ewz9xa3edyfmf622g5zjp8w"
                   , ccHotHex = "a9eb44d0aa1ce5559b7c22270ac23d1e61bf2e114dd8e5a44ed3a529"
                   , ccHotScript1 = "cc_hot1n42mr24e22eyspa7m0y6lq5rk8tesq35xt6gfgkezcxluez2snm"
                   , ccHotScript1Hex = "9d55b1aab952b24807bedbc9af8283b1d798023432f484a2d9160dfe"
                   , ccHotScript2 = "cc_hot1gfqmx4g0czk2nz2m2rfawg4me283jl7wz4wfssup03av2aynvs9"
                   , ccHotScript2Hex = "4241b3550fc0aca9895b50d3d722bbca8f197fce155c9843817c7ac5"
                   }
            }
    describe "Test vectors" $ do
        testVectors
            [ "test", "child", "burst", "immense", "armed", "parrot"
            , "company", "walk", "dog" ]
        testVectors
            [ "test", "walk", "nut", "penalty", "hip", "pave", "soap",
            "entry", "language", "right", "filter", "choice" ]
        testVectors
            [ "art", "forum", "devote", "street", "sure", "rather",
            "head", "chuckle", "guard", "poverty", "release",
            "quote", "oak", "craft", "enemy"]
        testVectors
            [ "churn", "shaft", "spoon", "second", "erode", "useless",
            "thrive", "burst", "group", "seed", "element", "sign",
            "scrub", "buffalo", "jelly", "grace", "neck", "useless" ]
        testVectors
            [ "draft", "ability", "female", "child", "jump", "maid",
            "roof", "hurt", "below", "live", "topple", "paper",
            "exclude", "ordinary", "coach", "churn", "sunset",
            "emerge", "blame", "ketchup", "much" ]
        testVectors
            [ "excess", "behave", "track", "soul", "table", "wear",
            "ocean", "cash", "stay", "nature", "item", "turtle",
            "palm", "soccer", "lunch", "horror", "start", "stumble",
            "month", "panic", "right", "must", "lock", "dress" ]

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

prop_publicChildKeyDerivation
    :: (SomeMnemonic, SndFactor)
    -> Role
    -> Index 'Soft 'PaymentK
    -> Property
prop_publicChildKeyDerivation (mw, (SndFactor sndFactor)) cc ix =
    addrXPub1 === addrXPub2
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv minBound
    addrXPub1 = toXPub <$> deriveAddressPrivateKey accXPrv cc ix
    addrXPub2 = deriveAddressPublicKey (toXPub <$> accXPrv) cc ix

prop_accountKeyDerivation
    :: (SomeMnemonic, SndFactor)
    -> Index 'Hardened 'AccountK
    -> Property
prop_accountKeyDerivation (mw, (SndFactor sndFactor)) ix =
    accXPrv `seq` property ()
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv = deriveAccountPrivateKey rootXPrv ix

prop_fromWord32Role :: Int -> Property
prop_fromWord32Role n =
    isNothing (toRole n)
    ===
    (n > fromRole maxBound || n < fromRole minBound)
  where
    fromRole = fromIntegral . indexToWord32 . roleToIndex
    toRole = roleFromIndex . unsafeMkIndex . fromIntegral

prop_roundtripIndexRole :: Role -> Property
prop_roundtripIndexRole ix = (roleFromIndex . roleToIndex) ix === Just ix

prop_pointerAddressConstruction
    :: (SomeMnemonic, SndFactor)
    -> Role
    -> Index 'Soft 'PaymentK
    -> NetworkDiscriminant Shelley
    -> ChainPointer
    -> Property
prop_pointerAddressConstruction (mw, (SndFactor sndFactor)) cc ix net ptr =
    pointerAddr `seq` property ()
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv minBound
    addrXPub = toXPub <$> deriveAddressPrivateKey accXPrv cc ix
    pointerAddr = pointerAddress net (PaymentFromExtendedKey addrXPub) ptr


{-------------------------------------------------------------------------------
                             Golden tests
-------------------------------------------------------------------------------}
-- | Definitions: Let's assume we have private key         < xprv | pub | cc >
-- Then, extended private key should be understood as      < xprv |     | cc >
--       extended public key  should be understood as      <      | pub | cc >
--       verification public key should be understood as   <      | pub |    >

data GoldenTestPointerAddress = GoldenTestPointerAddress
    {
      -- | The verification public key as string to be encoded into base16 form
      -- After that it should have 32-byte length
       verKey :: Text

      -- | Location of stake cerificate in the blockchain
    ,  stakePtr :: ChainPointer

      -- | Network tag
    ,  netTag :: Integer

      -- | Expected address as encoded into base16 form
    ,  expectedAddr :: Text
    }

goldenTestPointerAddress :: GoldenTestPointerAddress -> SpecWith ()
goldenTestPointerAddress GoldenTestPointerAddress{..} =
    it ("pointer address for networkId " <> show netTag) $ do
        let (Just xPub) = xpubFromBytes $ b16encode $ T.append verKey verKey
        let addrXPub = liftXPub xPub :: Shelley 'PaymentK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let ptrAddr = pointerAddress tag (PaymentFromExtendedKey addrXPub) stakePtr
        let (Right bytes) = b16decode expectedAddr
        ptrAddr `shouldBe` unsafeMkAddress bytes

data GoldenTestEnterpriseAddress = GoldenTestEnterpriseAddress
    {
      -- | The verification public key as string to be encoded into base16 form
      -- After that it should have 32-byte length
       verKey :: Text

      -- | Network tag
    ,  netTag :: Integer

      -- | Expected address as encoded into base16 form
    ,  expectedAddr :: Text
    }

goldenTestEnterpriseAddress :: GoldenTestEnterpriseAddress -> SpecWith ()
goldenTestEnterpriseAddress GoldenTestEnterpriseAddress{..} =
    it ("enterprise address for networkId " <> show netTag) $ do
        let (Just xPub) = xpubFromBytes $ b16encode $ T.append verKey verKey
        let addrXPub = liftXPub xPub :: Shelley 'PaymentK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let enterpriseAddr = Shelley.paymentAddress tag (PaymentFromExtendedKey addrXPub)
        let (Right bytes) = b16decode expectedAddr
        enterpriseAddr `shouldBe` unsafeMkAddress bytes

goldenTestEnterpriseAddressKeyHash :: GoldenTestEnterpriseAddress -> SpecWith ()
goldenTestEnterpriseAddressKeyHash GoldenTestEnterpriseAddress{..} =
    it ("enterprise address for networkId " <> show netTag) $ do
        let bs = b16encode $ T.append verKey verKey
        let (Just xPub) = xpubFromBytes bs
        let addrXPub = liftXPub xPub :: Shelley 'PaymentK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let enterpriseAddrFromKey =
                Shelley.paymentAddress tag (PaymentFromExtendedKey addrXPub)
        let keyHashDigest = hashCredential $ BS.take 32 bs
        let keyHash = KeyHash Payment keyHashDigest
        let enterpriseAddrFromKeyHash =
                Shelley.paymentAddress tag (PaymentFromKeyHash keyHash)
        enterpriseAddrFromKey `shouldBe` enterpriseAddrFromKeyHash

data GoldenTestBaseAddress = GoldenTestBaseAddress
    {
      -- | The verification public key as string to be encoded into base16 form
      -- After that it should have 32-byte length
       verKeyPayment :: Text

      -- | The verification public key as string to be encoded into base16 form
      -- After that it should have 32-byte length
    ,  verKeyStake :: Text

      -- | Network tag
    ,  netTag :: Integer

      -- | Expected address as encoded into base16 form
    ,  expectedAddr :: Text
    }

goldenTestBaseAddressPayFromXPub :: GoldenTestBaseAddress -> SpecWith ()
goldenTestBaseAddressPayFromXPub GoldenTestBaseAddress{..} =
    it ("base address for networkId " <> show netTag) $ do
        let (Just xPub1) =
                xpubFromBytes $ b16encode $ T.append verKeyPayment verKeyPayment
        let addrXPub = liftXPub xPub1 :: Shelley 'PaymentK XPub
        let (Just xPub2) =
                xpubFromBytes $ b16encode $ T.append verKeyStake verKeyStake
        let stakeXPub = liftXPub xPub2 :: Shelley 'DelegationK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let baseAddr =
                delegationAddress tag (PaymentFromExtendedKey addrXPub)
                (DelegationFromExtendedKey stakeXPub)
        let (Right bytes) = b16decode expectedAddr
        baseAddr `shouldBe` unsafeMkAddress bytes

goldenTestBaseAddressPayFromPub :: GoldenTestBaseAddress -> SpecWith ()
goldenTestBaseAddressPayFromPub GoldenTestBaseAddress{..} =
    it ("base address for networkId " <> show netTag) $ do
        let (Just pub1) =
                pubFromBytes $ b16encode verKeyPayment
        let addrPub = liftPub pub1 :: Shelley 'PaymentK Pub
        let (Just xPub2) =
                xpubFromBytes $ b16encode $ T.append verKeyStake verKeyStake
        let stakeXPub = liftXPub xPub2 :: Shelley 'DelegationK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let baseAddr =
                delegationAddress tag (PaymentFromKey addrPub)
                (DelegationFromExtendedKey stakeXPub)
        let (Right bytes) = b16decode expectedAddr
        baseAddr `shouldBe` unsafeMkAddress bytes

goldenTestBaseAddressPayFromKeyHash :: GoldenTestBaseAddress -> SpecWith ()
goldenTestBaseAddressPayFromKeyHash GoldenTestBaseAddress{..} =
    it ("base address for networkId " <> show netTag) $ do
        let paymentBs = b16encode $ T.append verKeyPayment verKeyPayment
        let (Just xPub1) = xpubFromBytes paymentBs
        let addrXPub = liftXPub xPub1 :: Shelley 'PaymentK XPub
        let (Just xPub2) =
                xpubFromBytes $ b16encode $ T.append verKeyStake verKeyStake
        let stakeXPub = liftXPub xPub2 :: Shelley 'DelegationK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let baseAddrPayFromKey =
                delegationAddress tag (PaymentFromExtendedKey addrXPub)
                (DelegationFromExtendedKey stakeXPub)
        let keyHashDigest = hashCredential $ BS.take 32 paymentBs
        let keyHash = KeyHash Payment keyHashDigest
        let baseAddrPayFromKeyHash =
                delegationAddress tag (PaymentFromKeyHash keyHash)
                (DelegationFromExtendedKey stakeXPub)
        baseAddrPayFromKey `shouldBe` baseAddrPayFromKeyHash

goldenTestBaseAddressStakeFromKeyHash :: GoldenTestBaseAddress -> SpecWith ()
goldenTestBaseAddressStakeFromKeyHash GoldenTestBaseAddress{..} =
    it ("base address for networkId " <> show netTag) $ do
        let paymentBs = b16encode $ T.append verKeyPayment verKeyPayment
        let (Just xPub1) = xpubFromBytes paymentBs
        let addrXPub = liftXPub xPub1 :: Shelley 'PaymentK XPub
        let stakeBs = b16encode $ T.append verKeyStake verKeyStake
        let (Just xPub2) = xpubFromBytes stakeBs
        let stakeXPub = liftXPub xPub2 :: Shelley 'DelegationK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let baseAddrStakeFromKey =
                delegationAddress tag (PaymentFromExtendedKey addrXPub)
                (DelegationFromExtendedKey stakeXPub)
        let keyHashDigest = hashCredential $ BS.take 32 stakeBs
        let keyHash = KeyHash Delegation keyHashDigest
        let baseAddrStakeFromKeyHash =
                delegationAddress tag (PaymentFromExtendedKey addrXPub)
                (DelegationFromKeyHash keyHash)
        baseAddrStakeFromKey `shouldBe` baseAddrStakeFromKeyHash

goldenTestBaseAddressStakeFromPub :: GoldenTestBaseAddress -> SpecWith ()
goldenTestBaseAddressStakeFromPub GoldenTestBaseAddress{..} =
    it ("base address for networkId " <> show netTag) $ do
        let paymentBs = b16encode $ T.append verKeyPayment verKeyPayment
        let (Just xPub1) = xpubFromBytes paymentBs
        let addrXPub = liftXPub xPub1 :: Shelley 'PaymentK XPub
        let stakeBs = b16encode $ T.append verKeyStake verKeyStake
        let (Just xPub2) = xpubFromBytes stakeBs
        let stakeXPub = liftXPub xPub2 :: Shelley 'DelegationK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let baseAddrStakeFromExtendedKey =
                delegationAddress tag (PaymentFromExtendedKey addrXPub)
                (DelegationFromExtendedKey stakeXPub)
        let stakeBs1 = b16encode verKeyStake
        let (Just pub2) = pubFromBytes stakeBs1
        let stakePub = liftPub pub2 :: Shelley 'DelegationK Pub
        let baseAddrStakeFromKey =
                delegationAddress tag (PaymentFromExtendedKey addrXPub)
                (DelegationFromKey stakePub)
        baseAddrStakeFromExtendedKey `shouldBe` baseAddrStakeFromKey

goldenTestBaseAddressBothFromKeyHash :: GoldenTestBaseAddress -> SpecWith ()
goldenTestBaseAddressBothFromKeyHash GoldenTestBaseAddress{..} =
    it ("base address for networkId " <> show netTag) $ do
        let paymentBs = b16encode $ T.append verKeyPayment verKeyPayment
        let (Just xPub1) = xpubFromBytes paymentBs
        let addrXPub = liftXPub xPub1 :: Shelley 'PaymentK XPub
        let stakeBs = b16encode $ T.append verKeyStake verKeyStake
        let (Just xPub2) = xpubFromBytes stakeBs
        let stakeXPub = liftXPub xPub2 :: Shelley 'DelegationK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let baseAddrBothFromKey =
                delegationAddress tag (PaymentFromExtendedKey addrXPub)
                (DelegationFromExtendedKey stakeXPub)
        let paymentKeyHashDigest = hashCredential $ BS.take 32 paymentBs
        let paymentKeyHash = KeyHash Payment paymentKeyHashDigest
        let stakeKeyHashDigest = hashCredential $ BS.take 32 stakeBs
        let stakeKeyHash = KeyHash Delegation stakeKeyHashDigest
        let baseAddrBothFromKeyHash =
                delegationAddress tag (PaymentFromKeyHash paymentKeyHash)
                (DelegationFromKeyHash stakeKeyHash)
        baseAddrBothFromKey `shouldBe` baseAddrBothFromKeyHash

goldenTestBaseAddressBothFromPub :: GoldenTestBaseAddress -> SpecWith ()
goldenTestBaseAddressBothFromPub GoldenTestBaseAddress{..} =
    it ("base address for networkId " <> show netTag) $ do
        let paymentBs = b16encode $ T.append verKeyPayment verKeyPayment
        let (Just xPub1) = xpubFromBytes paymentBs
        let addrXPub = liftXPub xPub1 :: Shelley 'PaymentK XPub
        let stakeBs = b16encode $ T.append verKeyStake verKeyStake
        let (Just xPub2) = xpubFromBytes stakeBs
        let stakeXPub = liftXPub xPub2 :: Shelley 'DelegationK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let baseAddrBothFromKey =
                delegationAddress tag (PaymentFromExtendedKey addrXPub)
                (DelegationFromExtendedKey stakeXPub)
        let paymentBs1 = b16encode verKeyPayment
        let (Just pub1) = pubFromBytes paymentBs1
        let addrPub = liftPub pub1 :: Shelley 'PaymentK Pub
        let stakeBs1 = b16encode verKeyStake
        let (Just pub2) = pubFromBytes stakeBs1
        let stakePub = liftPub pub2 :: Shelley 'DelegationK Pub
        let baseAddrBothFromPub =
                delegationAddress tag (PaymentFromKey addrPub)
                (DelegationFromKey stakePub)
        baseAddrBothFromKey `shouldBe` baseAddrBothFromPub

data KeysHashes = KeysHashes
    {  -- | CIP-1852’s DRep extended signing key (Ed25519-bip32 extended private key), bech32 encoded prefixed with 'drep_xsk'
      drepXsk :: Text

        -- | CIP-1852’s DRep extended signing key (Ed25519-bip32 extended private key), base16 encoded
    , drepXskHex :: Text

       -- | CIP-1852’s DRep signing key (Ed25519-bip32 non-extended private key), bech32 encoded prefixed with 'drep_sk'
    , drepSk :: Text

       -- | CIP-1852’s DRep signing key (Ed25519-bip32 non-extended private key), base16 encoded
    , drepSkHex :: Text

       -- | CIP-1852’s DRep extended verification key (Ed25519 public key with chain code), bech32 encoded prefixed with 'drep_xvk'
    , drepXvk :: Text

       -- | CIP-1852’s DRep extended verification key (Ed25519 public key with chain code), base16 encoded
    , drepXvkHex :: Text

       -- | CIP-1852’s DRep verification key (Ed25519 public key), bech32 encoded prefixed with 'drep_vk'
    , drepVk :: Text

       -- | CIP-1852’s DRep verification key (Ed25519 public key), base16 encoded
    , drepVkHex :: Text

       -- | Delegate representative verification key hash (DRep ID) (blake2b_224 digest of a delegate representative verification key), bech32 encoded prefixed with 'drep'
    , drep :: Text

       -- | Delegate representative verification key hash (DRep ID) (blake2b_224 digest of a delegate representative verification key), base16 encoded
    , drepHex :: Text

       -- | Delegate representative script hash (DRep ID) (blake2b_224 digest of a serialized delegate representative script prepended with 00100011 byte), bech32 encoded prefixed with 'drep'
       -- script: all [drep, active_from 5001]
    , drepScript1 :: Text

       -- | Delegate representative script hash (DRep ID) (blake2b_224 digest of a serialized delegate representative script), base16 encoded
       -- script: all [drep, active_from 5001]
    , drepScript1Hex :: Text

       -- | Delegate representative script hash (DRep ID) (blake2b_224 digest of a serialized delegate representative script prepended with 00100011 byte), bech32 encoded prefixed with 'drep'
       -- script: any [drep, all [active_from 5001, active_until 6001]]
    , drepScript2 :: Text

       -- | Delegate representative script hash (DRep ID) (blake2b_224 digest of a serialized delegate representative script), base16 encoded
       -- script: any [drep, all [active_from 5001, active_until 6001]]
    , drepScript2Hex :: Text

       -- | CIP-1852’s constitutional committee cold extended signing key (Ed25519-bip32 extended private key), bech32 encoded prefixed with 'cc_cold_xsk'
    , ccColdXsk :: Text

       -- | CIP-1852’s constitutional committee cold extended signing key (Ed25519-bip32 extended private key), base16 encoded
    , ccColdXskHex :: Text

       -- | CIP-1852’s constitutional committee cold signing key (Ed25519-bip32 non-extended private key), bech32 encoded prefixed with 'cc_cold_sk'
    , ccColdSk :: Text

       -- | CIP-1852’s constitutional committee cold signing key (Ed25519-bip32 non-extended private key), base16 encoded
    , ccColdSkHex :: Text

       -- | CIP-1852’s constitutional committee extended cold verification signing key (Ed25519 public key with chain code), bech32 encoded prefixed with 'cc_cold_xvk'
    , ccColdXvk :: Text

       -- | CIP-1852’s constitutional committee extended cold verification signing key (Ed25519 public key with chain code), base16 encoded
    , ccColdXvkHex :: Text

       -- | CIP-1852’s constitutional committee cold verification signing key (Ed25519 private key), bech32 encoded prefixed with 'cc_cold_vk'
    , ccColdVk :: Text

       -- | CIP-1852’s constitutional committee cold verification signing key (Ed25519 private key), base16 encoded
    , ccColdVkHex :: Text

       -- | Constitutional committee cold verification key hash (cold credential) (blake2b_224 digest of a consitutional committee cold verification key), bech32 encoded prefixed with 'cc_cold'
    , ccCold :: Text

       -- | Constitutional committee cold verification key hash (cold credential) (blake2b_224 digest of a consitutional committee cold verification key), base16 encoded
    , ccColdHex :: Text

       -- | Constitutional committee cold script hash (cold credential) (blake2b_224 digest of a serialized constitutional committee cold script prepended with 00010011 byte), bech32 encoded prefixed with 'cc_cold'
       -- script: all [ccCold, active_from 5001]
    , ccColdScript1 :: Text

       -- | Constitutional committee cold script hash (cold credential) (blake2b_224 digest of a serialized constitutional committee cold script), base16 encoded
       -- script: all [ccCold, active_from 5001]
    , ccColdScript1Hex :: Text

       -- | Constitutional committee cold script hash (cold credential) (blake2b_224 digest of a serialized constitutional committee cold script prepended with 00010011 byte), bech32 encoded prefixed with 'cc_cold'
       -- script: any [ccCold, all [active_from 5001, active_until 6001]]
    , ccColdScript2 :: Text

       -- | Constitutional committee cold script hash (cold credential) (blake2b_224 digest of a serialized constitutional committee cold script), base16 encoded
       -- script: any [ccCold, all [active_from 5001, active_until 6001]]
    , ccColdScript2Hex :: Text

       -- | CIP-1852’s constitutional committee hot extended signing key (Ed25519-bip32 extended private key), bech32 encoded prefixed with 'cc_hot_xsk'
    , ccHotXsk :: Text

       -- | CIP-1852’s constitutional committee hot extended signing key (Ed25519-bip32 extended private key), base16 encoded
    , ccHotXskHex :: Text

       -- | CIP-1852’s constitutional committee hot signing key (Ed25519-bip32 non-extended private key), bech32 encoded prefixed with 'cc_hot_sk'
    , ccHotSk :: Text

       -- | CIP-1852’s constitutional committee hot signing key (Ed25519-bip32 non-extended private key), base16 encoded
    , ccHotSkHex :: Text

       -- | CIP-1852’s constitutional committee extended hot verification signing key (Ed25519 public key with chain code), bech32 encoded prefixed with 'cc_hot_xvk'
    , ccHotXvk :: Text

       -- | CIP-1852’s constitutional committee extended hot verification signing key (Ed25519 public key with chain code), base16 encoded
    , ccHotXvkHex :: Text

       -- | CIP-1852’s constitutional committee hot verification signing key (Ed25519 private key), bech32 encoded prefixed with 'cc_hot_vk'
    , ccHotVk :: Text

       -- | CIP-1852’s constitutional committee hot verification signing key (Ed25519 private key), base16 encoded
    , ccHotVkHex :: Text

       -- | Constitutional committee hot verification key hash (hot credential) (blake2b_224 digest of a consitutional committee hot verification key), bech32 encoded prefixed with 'cc_hot'
    , ccHot :: Text

       -- | Constitutional committee hot verification key hash (hot credential) (blake2b_224 digest of a consitutional committee hot verification key), base16 encoded
    , ccHotHex :: Text

       -- | Constitutional committee hot script hash (hot credential) (blake2b_224 digest of a serialized constitutional committee hot script prepended with 00000011 byte), bech32 encoded prefixed with 'cc_hot'
       -- script: all [ccHot, active_from 5001]
    , ccHotScript1 :: Text

       -- | Constitutional committee hot script hash (hot credential) (blake2b_224 digest of a serialized constitutional committee hot script), base16 encoded
       -- script: all [ccHot, active_from 5001]
    , ccHotScript1Hex :: Text

       -- | Constitutional committee hot script hash (hot credential) (blake2b_224 digest of a serialized constitutional committee hot script prepended with 00000011 byte), bech32 encoded prefixed with 'cc_hot'
       -- script: any [ccHot, all [active_from 5001, active_until 6001]]
    , ccHotScript2 :: Text

           -- | Constitutional committee hot script hash (hot credential) (blake2b_224 digest of a serialized constitutional committee hot script), base16 encoded
       -- script: any [ccHot, all [active_from 5001, active_until 6001]]
    , ccHotScript2Hex :: Text

    } deriving (Eq, Show)

data GoldenTestGovernance = GoldenTestGovernance
    {
      -- | Mnemonic
      mnemonic :: [Text]

      -- | Account ix
    , accountIx :: Word32

      -- | Expected Keys and Hashes
    , expectedKeysHashes :: KeysHashes
    }

goldenTestGovernance :: GoldenTestGovernance -> SpecWith ()
goldenTestGovernance GoldenTestGovernance{..} =
    it ("governance keys/hashes for " <> show mnemonic <> " and accIx="<>show accountIx) $ do
        let (Right mw) = mkSomeMnemonic @'[9,12,15,18,21,24] mnemonic
        let sndFactor = mempty
        let rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv

        let Just accIx = indexFromWord32 @(Index 'Hardened _) accountIx
        let acctXPrv = deriveAccountPrivateKey rootXPrv accIx

        let drepXPrv = deriveDRepPrivateKey acctXPrv
        let drepXPrvTxt = bech32With CIP5.drep_xsk  $ getExtendedKeyAddr drepXPrv
        let drepXPrvTxtHex = tob16text . unAddress $ getExtendedKeyAddr drepXPrv
        let drepPrvTxt = bech32With CIP5.drep_sk  $ getPrivateKeyAddr drepXPrv
        let drepPrvTxtHex = T.decodeUtf8 $ encode EBase16 $ unAddress $ getPrivateKeyAddr drepXPrv
        let drepXPubTxt = bech32With CIP5.drep_xvk $ getPublicKeyAddr $ toXPub <$> drepXPrv
        let drepXPubTxtHex = tob16text . unAddress $ getPublicKeyAddr $ toXPub <$> drepXPrv
        let drepPubTxt = bech32With CIP5.drep_vk $ getVerKey $ toXPub <$> drepXPrv
        let drepPubTxtHex = tob16text . unAddress $ getVerKey $ toXPub <$> drepXPrv
        let drepKeyHash = toKeyHash Representative $ toXPub <$> drepXPrv
        let drepTxt = keyHashToText drepKeyHash CIP0129
        let drepTxtHex = tob16text . digest $ drepKeyHash
        let drepScriptHash1 = toScriptHash (script1 drepKeyHash)
        let drepScript1Txt = toScriptTxt drepScriptHash1 CIP5.drep
        let drepScript1TxtHex = tob16text . unScriptHash $ drepScriptHash1
        let drepScriptHash2 = toScriptHash (script2 drepKeyHash)
        let drepScript2Txt = toScriptTxt drepScriptHash2 CIP5.drep
        let drepScript2TxtHex = tob16text . unScriptHash $ drepScriptHash2

        let coldXPrv = deriveCCColdPrivateKey acctXPrv
        let coldXPrvTxt = bech32With CIP5.cc_cold_xsk $ getExtendedKeyAddr coldXPrv
        let coldXPrvTxtHex = tob16text . unAddress $ getExtendedKeyAddr coldXPrv
        let coldPrvTxt = bech32With CIP5.cc_cold_sk  $ getPrivateKeyAddr coldXPrv
        let coldPrvTxtHex = T.decodeUtf8 $ encode EBase16 $ unAddress $ getPrivateKeyAddr coldXPrv
        let coldXPubTxt = bech32With CIP5.cc_cold_xvk $ getPublicKeyAddr $ toXPub <$> coldXPrv
        let coldXPubTxtHex = tob16text . unAddress  $ getPublicKeyAddr $ toXPub <$> coldXPrv
        let coldPubTxt = bech32With CIP5.cc_cold_vk $ getVerKey $ toXPub <$> coldXPrv
        let coldPubTxtHex = tob16text . unAddress $ getVerKey $ toXPub <$> coldXPrv
        let coldKeyHash = toKeyHash CommitteeCold $ toXPub <$> coldXPrv
        let coldTxt = keyHashToText coldKeyHash CIP0129
        let coldTxtHex = tob16text . digest $ coldKeyHash
        let coldScriptHash1 = toScriptHash (script1 coldKeyHash)
        let coldScript1Txt = toScriptTxt coldScriptHash1 CIP5.cc_cold
        let coldScript1TxtHex = tob16text . unScriptHash $ coldScriptHash1
        let coldScriptHash2 = toScriptHash (script2 coldKeyHash)
        let coldScript2Txt = toScriptTxt coldScriptHash2 CIP5.cc_cold
        let coldScript2TxtHex = tob16text . unScriptHash $ coldScriptHash2

        let hotXPrv = deriveCCHotPrivateKey acctXPrv
        let hotXPrvTxt = bech32With CIP5.cc_hot_xsk  $ getExtendedKeyAddr hotXPrv
        let hotXPrvTxtHex = tob16text . unAddress $ getExtendedKeyAddr hotXPrv
        let hotPrvTxt = bech32With CIP5.cc_hot_sk  $ getPrivateKeyAddr hotXPrv
        let hotPrvTxtHex = T.decodeUtf8 $ encode EBase16 $ unAddress $ getPrivateKeyAddr hotXPrv
        let hotXPubTxt = bech32With CIP5.cc_hot_xvk $ getPublicKeyAddr $ toXPub <$> hotXPrv
        let hotXPubTxtHex = tob16text . unAddress  $ getPublicKeyAddr $ toXPub <$> hotXPrv
        let hotPubTxt = bech32With CIP5.cc_hot_vk $ getVerKey $ toXPub <$> hotXPrv
        let hotPubTxtHex = tob16text . unAddress $ getVerKey $ toXPub <$> hotXPrv
        let hotKeyHash = toKeyHash CommitteeHot $ toXPub <$> hotXPrv
        let hotTxt = keyHashToText hotKeyHash CIP0129
        let hotTxtHex = tob16text . digest $ hotKeyHash
        let hotScriptHash1 = toScriptHash (script1 hotKeyHash)
        let hotScript1Txt = toScriptTxt hotScriptHash1 CIP5.cc_hot
        let hotScript1TxtHex = tob16text . unScriptHash $ hotScriptHash1
        let hotScriptHash2 = toScriptHash (script2 hotKeyHash)
        let hotScript2Txt = toScriptTxt hotScriptHash2 CIP5.cc_hot
        let hotScript2TxtHex = tob16text . unScriptHash $ hotScriptHash2

        let derivedKeysHashes = KeysHashes
                { drepXsk = drepXPrvTxt
                , drepXskHex = drepXPrvTxtHex
                , drepSk = drepPrvTxt
                , drepSkHex = drepPrvTxtHex
                , drepXvk = drepXPubTxt
                , drepXvkHex = drepXPubTxtHex
                , drepVk = drepPubTxt
                , drepVkHex = drepPubTxtHex
                , drep = drepTxt
                , drepHex = drepTxtHex
                , drepScript1 = drepScript1Txt
                , drepScript1Hex = drepScript1TxtHex
                , drepScript2 = drepScript2Txt
                , drepScript2Hex = drepScript2TxtHex
                , ccColdXsk = coldXPrvTxt
                , ccColdXskHex = coldXPrvTxtHex
                , ccColdSk = coldPrvTxt
                , ccColdSkHex = coldPrvTxtHex
                , ccColdXvk = coldXPubTxt
                , ccColdXvkHex = coldXPubTxtHex
                , ccColdVk = coldPubTxt
                , ccColdVkHex = coldPubTxtHex
                , ccCold = coldTxt
                , ccColdHex = coldTxtHex
                , ccColdScript1 = coldScript1Txt
                , ccColdScript1Hex = coldScript1TxtHex
                , ccColdScript2 = coldScript2Txt
                , ccColdScript2Hex = coldScript2TxtHex
                , ccHotXsk = hotXPrvTxt
                , ccHotXskHex = hotXPrvTxtHex
                , ccHotSk = hotPrvTxt
                , ccHotSkHex = hotPrvTxtHex
                , ccHotXvk = hotXPubTxt
                , ccHotXvkHex = hotXPubTxtHex
                , ccHotVk = hotPubTxt
                , ccHotVkHex = hotPubTxtHex
                , ccHot = hotTxt
                , ccHotHex = hotTxtHex
                , ccHotScript1 = hotScript1Txt
                , ccHotScript1Hex = hotScript1TxtHex
                , ccHotScript2 = hotScript2Txt
                , ccHotScript2Hex = hotScript2TxtHex
                }
        derivedKeysHashes `shouldBe` expectedKeysHashes
  where
    getVerKey = unsafeMkAddress . pubToBytes . xpubToPub . getKey
    toKeyHash role =
          KeyHash role
        . hashCredential
        . pubToBytes
        . xpubToPub
        . getKey
    script1 keyhash =
        RequireAllOf [RequireSignatureOf keyhash, ActiveFromSlot 5001]
    script2 keyhash =
        RequireAnyOf [ RequireSignatureOf keyhash
                     , RequireAllOf [ ActiveFromSlot 5001, ActiveUntilSlot 6001]
                     ]
    toScriptTxt (ScriptHash bytes) hrp =
        bech32With hrp $
        unsafeMkAddress bytes

data TestVector = TestVector
    {
      -- | The extended root private key, bech32 encoded prefixed with 'root_xsk'
      rootXPrv :: Text

      -- | The extended 0th account private key, bech32 encoded prefixed with 'acct_xsk'
    , accXPrv0 :: Text

      -- | The extended 1st account private key, bech32 encoded prefixed with 'acct_xsk'
    , accXPrv1 :: Text

      -- | The extended 0th address private key, bech32 encoded prefixed with 'addr_xsk'
    , addrXPrv0 :: Text

      -- | The extended 0th address public key, bech32 encoded prefixed with 'addr_xvk'
    , addrXPub0 :: Text

      -- | The extended 1st address private key, bech32 encoded prefixed with 'addr_xsk'
    , addrXPrv1 :: Text

      -- | The extended 1st address public key, bech32 encoded prefixed with 'addr_xvk'
    , addrXPub1 :: Text

      -- | The extended 1442nd address private key, bech32 encoded prefixed with 'addr_xsk'
    , addrXPrv1442 :: Text

      -- | The extended 1442nd address public key, bech32 encoded prefixed with 'addr_xvk'
    , addrXPub1442 :: Text

      -- | The payment address for 0th address key, with a networking tag 0, 3 and 6, respectively.
      -- Each bech32 encoded prefixed with 'addr'
    , paymentAddr0 :: [Text]

      -- | The payment address for 1st address key, with a networking tag 0, 3 and 6, respectively.
      -- Each bech32 encoded prefixed with 'addr'
    , paymentAddr1 :: [Text]

      -- | The payment address for 1442nd address key, with a networking tag 0, 3 and 6, respectively.
      -- Each bech32 encoded prefixed with 'addr'
    , paymentAddr1442 :: [Text]

      -- | Delegation addresses for the 0th address key and the 0th account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr0Stake0 :: [Text]

      -- | Delegation addresses for the 1st address key and the 0th account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr1Stake0 :: [Text]

      -- | Delegation addresses for the 1442nd address key and the 0th account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr1442Stake0 :: [Text]

      -- | Delegation addresses for the 0th address key and the 1st account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr0Stake1 :: [Text]

      -- | Delegation addresses for the 1st address key and the 1st account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr1Stake1 :: [Text]

      -- | Delegation addresses for the 1442nd address key and the 1st account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr1442Stake1 :: [Text]

      -- | Pointer addresses for the 0th address key and the delegation certificate located in slot 1,
      -- transaction index 2, certificte list index 3, with a networking tag 0, 3 and 6, respectively.
      -- Each bech32 encoded prefixed with 'addr'
     , pointerAddr0Slot1 :: [Text]

      -- | Pointer addresses for the 0th address key and the delegation certificate located in slot 24157,
      -- transaction index 177, certificte list index 42, with a networking tag 0, 3 and 6, respectively.
      -- Each bech32 encoded prefixed with 'addr'
     , pointerAddr0Slot2 :: [Text]

      -- | Corresponding Mnemonic
    , mnemonic :: [Text]

    } deriving Show

data InspectVector a = InspectVector
    { addrXPub0 :: a
    , addrXPub1 :: a
    , addrXPub1442 :: a
    , paymentAddr0 :: [a]
    , paymentAddr1 :: [a]
    , paymentAddr1442 :: [a]
    , delegationAddr0Stake0 :: [a]
    , delegationAddr1Stake0 :: [a]
    , delegationAddr1442Stake0 :: [a]
    , delegationAddr0Stake1 :: [a]
    , delegationAddr1Stake1 :: [a]
    , delegationAddr1442Stake1 :: [a]
    , pointerAddr0Slot1 :: [a]
    , pointerAddr0Slot2 :: [a]
    } deriving (Generic, Show, Functor)
    deriving anyclass ToJSON

testVectors :: [Text] -> SpecWith ()
testVectors mnemonic = describe (show $ T.unpack <$> mnemonic) $ do
    let (Right mw) = mkSomeMnemonic @'[9,12,15,18,21,24] mnemonic
    let sndFactor = mempty
    let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    let rootXPrv = bech32With CIP5.root_xsk $ getExtendedKeyAddr rootK

    let Just accIx0 = indexFromWord32 @(Index 'Hardened _) 0x80000000
    let acctK0 = deriveAccountPrivateKey rootK accIx0
    let accXPrv0 = bech32With CIP5.acct_xsk  $ getExtendedKeyAddr acctK0
    let Just accIx1 = indexFromWord32 @(Index 'Hardened _) 0x80000001
    let acctK1 = deriveAccountPrivateKey rootK accIx1
    let accXPrv1 = bech32With CIP5.acct_xsk $ getExtendedKeyAddr acctK1

    let Just addIx0 = indexFromWord32 @(Index 'Soft _) 0x00000000
    let addrK0prv = deriveAddressPrivateKey acctK0 UTxOExternal addIx0
    let addrXPrv0 = bech32With CIP5.addr_xsk $ getExtendedKeyAddr addrK0prv
    let addrXPub0 = bech32With CIP5.addr_xvk $ getPublicKeyAddr $ toXPub <$> addrK0prv

    let Just addIx1 = indexFromWord32 @(Index 'Soft _) 0x00000001
    let addrK1prv = deriveAddressPrivateKey acctK0 UTxOExternal addIx1
    let addrXPrv1 = bech32With CIP5.addr_xsk $ getExtendedKeyAddr addrK1prv
    let addrXPub1 = bech32With CIP5.addr_xvk $ getPublicKeyAddr $ toXPub <$> addrK1prv
    let Just addIx1442 = indexFromWord32 @(Index 'Soft _) 0x000005a2
    let addrK1442prv = deriveAddressPrivateKey acctK0 UTxOExternal addIx1442
    let addrXPrv1442 = bech32With CIP5.addr_xsk $ getExtendedKeyAddr addrK1442prv
    let addrXPub1442 = bech32With CIP5.addr_xvk $ getPublicKeyAddr $ toXPub <$> addrK1442prv

    let networkTags = rights $ mkNetworkDiscriminant <$> [0,3,6]
    let paymentAddr0 = getPaymentAddr addrK0prv <$> networkTags
    let paymentAddr1 = getPaymentAddr addrK1prv <$> networkTags
    let paymentAddr1442 = getPaymentAddr addrK1442prv <$> networkTags

    let slot1 = ChainPointer 1 2 3
    let pointerAddr0Slot1 = getPointerAddr addrK0prv slot1 <$> networkTags
    let slot2 = ChainPointer 24157 177 42
    let pointerAddr0Slot2 = getPointerAddr addrK0prv slot2 <$> networkTags

    let stakeKPub0 = toXPub <$> deriveDelegationPrivateKey acctK0
    let delegationAddr0Stake0 = getDelegationAddr addrK0prv (DelegationFromExtendedKey stakeKPub0) <$> networkTags
    let delegationAddr1Stake0 = getDelegationAddr addrK1prv (DelegationFromExtendedKey stakeKPub0) <$> networkTags
    let delegationAddr1442Stake0 = getDelegationAddr addrK1442prv (DelegationFromExtendedKey stakeKPub0) <$> networkTags
    let stakeKPub1 = toXPub <$> deriveDelegationPrivateKey acctK1
    let delegationAddr0Stake1 = getDelegationAddr addrK0prv (DelegationFromExtendedKey stakeKPub1) <$> networkTags
    let delegationAddr1Stake1 = getDelegationAddr addrK1prv (DelegationFromExtendedKey stakeKPub1) <$> networkTags
    let delegationAddr1442Stake1 = getDelegationAddr addrK1442prv (DelegationFromExtendedKey stakeKPub1) <$> networkTags
    let vec = TestVector {..}
    let inspectVec = InspectVector {..}
    it "should generate correct addresses" $ do
        goldenTextLazy ("addresses_" <> shortHex (T.intercalate "_" mnemonic))
            (pShowOpt defaultOutputOptionsNoColor vec)
    it "should inspect correctly" $ do
        goldenByteStringLazy ("inspects_" <> shortHex (T.intercalate "_" mnemonic)) $
            goldenEncodeJSON $ toInspect <$> inspectVec
  where
    getPaymentAddr addrKPrv net =  bech32 $ paymentAddress net (PaymentFromExtendedKey (toXPub <$> addrKPrv))
    getPointerAddr addrKPrv ptr net =  bech32 $ pointerAddress net (PaymentFromExtendedKey (toXPub <$> addrKPrv)) ptr
    getDelegationAddr addrKPrv stakeKPub net =
        bech32 $ delegationAddress net (PaymentFromExtendedKey (toXPub <$> addrKPrv)) stakeKPub
    toInspect :: Text -> Value
    toInspect = fromMaybe (String "couldn't inspect") .
        (Shelley.inspectAddress Nothing <=< fromBech32)

-- | Make a short name for a mnemonic by hashing it and taking the first 8 characters
shortHex :: Text -> Text
shortHex = T.take 8 . T.decodeUtf8 . BAE.convertToBase BAE.Base16 . hashByteString . T.encodeUtf8
  where
    hashByteString :: ByteString -> ByteString
    hashByteString = BA.convert . hashWith SHA3_256

getExtendedKeyAddr :: Shelley depth XPrv -> Address
getExtendedKeyAddr = unsafeMkAddress . xprvToBytes . getKey

getPrivateKeyAddr :: Shelley depth XPrv -> Address
getPrivateKeyAddr = unsafeMkAddress . xprvPrivateKey . getKey

getPublicKeyAddr :: Shelley depth XPub -> Address
getPublicKeyAddr = unsafeMkAddress . xpubToBytes . getKey

goldenEncodeJSON :: ToJSON a => a -> BL.ByteString
goldenEncodeJSON = Aeson.encodePretty' cfg
  where
    cfg = Aeson.defConfig { Aeson.confCompare = compare }

prop_roundtripTextEncoding
    :: (Address -> Text)
        -- ^ encode to 'Text'
    -> (Text -> Maybe Address)
        -- ^ decode from 'Text'
    -> Shelley 'PaymentK XPub
        -- ^ An arbitrary public key
    -> NetworkDiscriminant Shelley
        -- ^ An arbitrary network discriminant
    -> Property
prop_roundtripTextEncoding encode' decode addXPub discrimination =
    (result == pure address)
        & counterexample (unlines
            [ "Address " <> T.unpack (encode' address)
            , "↳       " <> maybe "ø" (T.unpack . encode') result
            ])
        & label (show $ addressDiscrimination @Shelley discrimination)
  where
    address = paymentAddress discrimination (PaymentFromExtendedKey addXPub)
    result  = decode (encode' address)

prop_roundtripTextEncodingDelegation
    :: (Address -> Text)
        -- ^ encode to 'Text'
    -> (Text -> Maybe Address)
        -- ^ decode from 'Text'
    -> Shelley 'PaymentK XPub
        -- ^ An arbitrary address public key
    -> Shelley 'DelegationK XPub
        -- ^ An arbitrary delegation public key
    -> NetworkDiscriminant Shelley
        -- ^ An arbitrary network discriminant
    -> Property
prop_roundtripTextEncodingDelegation encode' decode addXPub delegXPub discrimination =
    (result == pure address)
        & counterexample (unlines
            [ "Address " <> T.unpack (encode' address)
            , "↳       " <> maybe "ø" (T.unpack . encode') result
            ])
        & label (show $ addressDiscrimination @Shelley discrimination)
  where
    address = delegationAddress discrimination (PaymentFromExtendedKey addXPub) (DelegationFromExtendedKey delegXPub)
    result  = decode (encode' address)

prop_roundtripTextEncodingPointer
    :: (Address -> Text)
        -- ^ encode to 'Text'
    -> (Text -> Maybe Address)
        -- ^ decode from 'Text'
    -> Shelley 'PaymentK XPub
        -- ^ An arbitrary address public key
    -> ChainPointer
        -- ^ An arbitrary delegation key locator
    -> NetworkDiscriminant Shelley
        -- ^ An arbitrary network discriminant
    -> Property
prop_roundtripTextEncodingPointer encode' decode addXPub ptr discrimination =
    (result == pure address)
        & counterexample (unlines
            [ "Address " <> T.unpack (encode' address)
            , "↳       " <> maybe "ø" (T.unpack . encode') result
            ])
        & label (show $ addressDiscrimination @Shelley discrimination)
  where
    address = pointerAddress discrimination (PaymentFromExtendedKey addXPub) ptr
    result  = decode (encode' address)

goldenTextLazy :: Text -> TL.Text -> Golden TL.Text
goldenTextLazy name output =
    Golden
    { output = output
    , encodePretty = TL.unpack
    , writeToFile = TL.writeFile
    , readFromFile = TL.readFile
    , testName = T.unpack name
    , directory = "test/golden"
    , failFirstTime = False
    }

goldenByteStringLazy :: Text -> BL.ByteString -> Golden BL.ByteString
goldenByteStringLazy name output =
    Golden
    { output = output
    , encodePretty = TL.unpack . TLE.decodeUtf8
    , writeToFile = \ fp -> BL.writeFile fp . normalizeLBS
    , readFromFile = fmap normalizeLBS . BL.readFile
    , testName = T.unpack name
    , directory = "test/golden"
    , failFirstTime = False
    }

-- This should be sufficent to convert Windows "\r\n" newlines to UNIX "\n" newlines
-- as used by both Mac and Linux.
normalizeLBS :: BL.ByteString -> BL.ByteString
normalizeLBS = BL.filter (/= fromIntegral (Char.ord '\r'))

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

newtype SndFactor = SndFactor ScrubbedBytes
    deriving stock (Eq, Show)
    deriving newtype (ByteArrayAccess)

instance Arbitrary SndFactor where
    arbitrary = do
        n <- choose (0, 64)
        bytes <- BS.pack <$> vector n
        return $ SndFactor $ BA.convert bytes

tob16text :: ByteString -> Text
tob16text =  T.decodeUtf8 . encode EBase16

b16encode :: Text -> ByteString
b16encode = encode EBase16 . T.encodeUtf8

b16decode :: Text -> Either String ByteString
b16decode = fromBase16 . T.encodeUtf8
