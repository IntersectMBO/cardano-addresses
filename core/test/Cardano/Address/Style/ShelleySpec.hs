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
    , xprvToBytes
    , xpubFromBytes
    , xpubToBytes
    , xpubToPub
    )
import Cardano.Address.Script
    ( KeyHash (..), KeyRole (..) )
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
                   , drepXvk = "drep_xvk17axh4sc9zwkpsft3tlgpjemfwc0u5mnld80r85zw7zdqcst6w543mpq3q2vkjy3nw8x7n8asw4es78dyl4q7u7kwlwn7yy0sugxfrjs6z25qe"
                   , drepVk = "drep_vk17axh4sc9zwkpsft3tlgpjemfwc0u5mnld80r85zw7zdqcst6w54sdv4a4e"
                   , drep = "drep15k6929drl7xt0spvudgcxndryn4kmlzpk4meed0xhqe25nle07s"
                   , ccColdXsk = "cc_cold_xsk1dp84kjq9qa647wr70e2yedzt8e27kwugh8mfw675re0hgm8p530z3d9230cjjzyyzlq04hn94x9q2m9um2tvp2y8fn7tau9l2wfj5ykxqxtgua0lxpf0lfn44md2afyl7dktyvpkmug9u28p6v452flxeuca0v7w"
                   , ccColdXvk = "cc_cold_xvk149up407pvp9p36lldlp4qckqqzn6vm7u5yerwy8d8rqalse3t04vvqvk3e6l7vzjl7n8ttk646jflumvkgcrdhcstc5wr5etg5n7dnc8nqv5d"
                   , ccColdVk = "cc_cold_vk149up407pvp9p36lldlp4qckqqzn6vm7u5yerwy8d8rqalse3t04q7qsvwl"
                   , cc_cold = "cc_cold1lmaet9hdvu9d9jvh34u0un4ndw3yewaq5ch6fnwsctw02xxwylj"
                   , ccHotXsk = "cc_hot_xsk1mpt30ys7v2ykqms4c83wuednh4hvy3lr27yfhgtp0rhdka8p5300j4d2z77sq2t3kp082qzgkanwkm05mp2u2nwja3ad3pgw9l34a0j5sl5yd6d8pze8dqwksd069kkfdqggk0yytcmet96fre45w64qkgyxl0dt"
                   , ccHotXvk = "cc_hot_xvk10y48lq72hypxraew74lwjjn9e2dscuwphckglh2nrrpkgweqk5h4fplggm56wz9jw6qadq6l5tdvj6qs3v7ggh3hjkt5j8ntga42pvs5rvh0a"
                   , ccHotVk = "cc_hot_vk10y48lq72hypxraew74lwjjn9e2dscuwphckglh2nrrpkgweqk5hschnzv5"
                   , cc_hot = "cc_hot17mffcrm3vnfhvyxt7ea3y65e804jfgrk6pjn78aqd9vg7xpq8dv"
                   }
            }
        goldenTestGovernance GoldenTestGovernance
            {  mnemonic = [ "test", "walk", "nut", "penalty", "hip", "pave", "soap",
                            "entry", "language", "right", "filter", "choice" ]
            ,  accountIx = 0x80000100
            ,  expectedKeysHashes = KeysHashes
                   { drepXsk = "drep_xsk1zracgd4mqt32f5cj0ps0wudf78u6lumz7gprgm3j8zec5ahp530weq4z9ayj6jzj33lpj86jkk2gnt0ns0d5sywteexxehvva7gugz99ydmpemzpsfnj49vjvw88q9a2s2hxc9ggxal5q6xsqz5vaat2xqsha72w"
                   , drepXvk = "drep_xvk1wq6ylcpjnwavhveey855tkhdrqdav6yfxvltw0emky9d3erxn9m22gmkrnkyrqn8922eycuwwqt64q4wds2ssdmlgp5dqq9gem6k5vq23ph3c"
                   , drepVk = "drep_vk1wq6ylcpjnwavhveey855tkhdrqdav6yfxvltw0emky9d3erxn9mqdrlerg"
                   , drep = "drep1rmf3ftma8lu0e5eqculttpfy6a6v5wrn8msqa09gr0tr5rgcuy9"
                   , ccColdXsk = "cc_cold_xsk1dppxrjspxrjj5e5xrmh6yaw6w30arsl5lqcsp09ynyzwwulp530q4tlvug79xx6ja3u32fu9jyy84p6erjmza6twrackm9kfsdpc3ap7uxpempqjftx74qwxnmn7d6pg8pl9zpnc0rese26pfmzl9cmtgg8xsxvu"
                   , ccColdXvk = "cc_cold_xvk1e2mquwugpwnykfftjs4mv3w4uk80f4hjgd2zls5vusz3zuqhr7gnacvrnkzpyjkda2qud8h8um5zswr72yr8s78npj45znk97t3kkssryhkyv"
                   , ccColdVk = "cc_cold_vk1e2mquwugpwnykfftjs4mv3w4uk80f4hjgd2zls5vusz3zuqhr7gs3qg4hr"
                   , cc_cold = "cc_cold1aymnf7h8rr53h069ephcekq707tg0ek0lexfzrw35npkq02wke0"
                   , ccHotXsk = "cc_hot_xsk15pt89wppyhr9eqgm5nnu7tna3dfmqxa2u45e4g7krzp9u78p530pez36k8k9n0gw08hn6drxlwxxsgc4jsejv6hvcnkd7gd3zxhstpe3vzde6e98zql6n2cmekklm63dydnt80szdr0h768dexeklrfspc5lznuz"
                   , ccHotXvk = "cc_hot_xvk10qawpxlz7eytt9yr4xlwtjkw345v0ehzsxdlkks6qralyp975phrzcymn4j2wypl4x43hnddlh4z6gmxkwlqy6xl0a5wmjdnd7xnqrsvak8ry"
                   , ccHotVk = "cc_hot_vk10qawpxlz7eytt9yr4xlwtjkw345v0ehzsxdlkks6qralyp975phqx538xn"
                   , cc_hot = "cc_hot1682whkcedz0ftcyhjxdasufyg85fks0vxm0y006qx38c2jz0ae0"
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
                   , drepXvk = "drep_xvk15j30gk0uex88lc9vh6sfda93lv6zede65mzp7ck56m9pgeqhnht9l4s7m9tad59jmltv3c38nclt942n3feen6ggmhcj6xmmlj6td2qu4ce82"
                   , drepVk = "drep_vk15j30gk0uex88lc9vh6sfda93lv6zede65mzp7ck56m9pgeqhnhtqvs6j8t"
                   , drep = "drep1x0jc06clgnj37sc8amkhahnpjqytcnguxtqcpxwkxeejj4y6sqm"
                   , ccColdXsk = "cc_cold_xsk1hqtevrzlhtcglwvt5pmgct8ssqx37vjjf3wuydpd6flyqrg33azacap5w5mclacmuycx3xgrtstxgrpzcncf6l840t0klmywc69ryd9zf95taaaseka98yakuj2048slnuekw22qm58majt8alhs438eecehquu0"
                   , ccColdXvk = "cc_cold_xvk13wc4cvvr266t4rxm9wyel4deeqxyylvjzjdk5w74lva2xm0dhxt6yjtghmmmpnd62wfmdey5l20pl8envu55phg0hmyk0ml0ptz0nns9cqjlk"
                   , ccColdVk = "cc_cold_vk13wc4cvvr266t4rxm9wyel4deeqxyylvjzjdk5w74lva2xm0dhxtsfpa2qu"
                   , cc_cold = "cc_cold17z4s83htmrgmg5268hx68j4vqumk38wrc5x9cr0mc7glyntw6cl"
                   , ccHotXsk = "cc_hot_xsk1wzamzchtj7m79mjfpg3c02m534ugej5ac0p3s2sresr7vys33azktmjva6flctprqu6m4k4w459x9qkfsz2ahgy5ganjn23djhhkg5e5eyhu7fjxl6tpxtmzh7e2ftuj4qgmawsmcl7sqesn8e0pmh97zs3c3fqj"
                   , ccHotXvk = "cc_hot_xvk1tazd6lvnf2c9j8m58h6xy56uuyhkee526jgxj2ylaextl0xamd4nfjf0eunydl5kzvhk90aj5jhe92q3h6aph3laqpnpx0j7rhwtu9qe7dhsc"
                   , ccHotVk = "cc_hot_vk1tazd6lvnf2c9j8m58h6xy56uuyhkee526jgxj2ylaextl0xamd4swmuygc"
                   , cc_hot = "cc_hot1c2n5ax72vfqdj3ljn04hmmvkqjt5q9k694yw8f7rv3xvgxas90x"
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
                   , drepXvk = "drep_xvk14dwjrplj73qeggdsg4lh4j9tp495asyq9t6augwaue8kqvjg5wq4wxstfkf8waakk8pwv8fkrde2cwd47rklfx9xxpn9ulc7nl7sndcvdjh2m"
                   , drepVk = "drep_vk14dwjrplj73qeggdsg4lh4j9tp495asyq9t6augwaue8kqvjg5wqskrq5yn"
                   , drep = "drep1cx359uxlhq4e8j3wddqxht9sfqp004t2n8v0jk5q4zmv27sh0h5"
                   , ccColdXsk = "cc_cold_xsk1hqe5kcsq59mx4t9nxrctmth0ppz9gda0gnppyll3h9rxcyq33az4uy3u6qhzuhjsstzca9awgsx27j07hxhrkrk6487nvywp0ag669m4v6lj3knq7e6pxaujy98akn5exhgk44ftruepkte0hdm74dd8zceqnk2h"
                   , ccColdXvk = "cc_cold_xvk1lmqejccjpxsd9cl4uavxj0jryjlfk5r8wemr0d8saal49lttp2482e4l9rdxpan5zdmeyg20md8fjdw3dt2jk8ejrvhjlwmha266w9syf55nr"
                   , ccColdVk = "cc_cold_vk1lmqejccjpxsd9cl4uavxj0jryjlfk5r8wemr0d8saal49lttp24q6lw08l"
                   , cc_cold = "cc_cold1fjej4ec9lvam509vjapr26yqeyf2x6j20n98f4y4d3l5zygwxt4"
                   , ccHotXsk = "cc_hot_xsk14rzh5lvtdhvum6vjfvkwp73mz9gl426cj04xfavnjgmdxrq33azugz0k9sekf2eg70lr34rg5aclr54v30za77xn945kncdm0le6lutxlr5ar355u5awqt2hkmdurv4qv64cmpg39zq2ahjxqken8vk62qunx4hl"
                   , ccHotXvk = "cc_hot_xvk1g2925ntunmthw66sr8t7v3qe7fls4575wput3936cguzk7m6w4fkd78f68rffef6uqk40dkmcxe2qe4t3kz3z2yq4m0yvpdnxwed55q798msd"
                   , ccHotVk = "cc_hot_vk1g2925ntunmthw66sr8t7v3qe7fls4575wput3936cguzk7m6w4fs0zjxf8"
                   , cc_hot = "cc_hot14845f592rnj4txmuygns4s3aresm7ts3fhvwtfzw6wjjj3l0520"
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

--TO_DO add script golden
data KeysHashes = KeysHashes
    {  -- | CIP-1852’s DRep extended signing key (Ed25519-bip32 extended private key), bech32 encoded prefixed with 'drep_xsk'
      drepXsk :: Text

       -- | CIP-1852’s DRep extended verification key (Ed25519 public key with chain code), bech32 encoded prefixed with 'drep_xvk'
    , drepXvk :: Text

       -- | CIP-1852’s DRep verification key (Ed25519 public key), bech32 encoded prefixed with 'drep_vk'
    , drepVk :: Text

       -- | Delegate representative verification key hash (DRep ID) (blake2b_224 digest of a delegate representative verification key), , bech32 encoded prefixed with 'drep'
    , drep :: Text

       -- | CIP-1852’s constitutional committee cold extended signing key (Ed25519-bip32 extended private key), bech32 encoded prefixed with 'cc_cold_xsk'
    , ccColdXsk :: Text

       -- | CIP-1852’s constitutional committee extended cold verification signing key (Ed25519 public key with chain code), bech32 encoded prefixed with 'cc_cold_xvk'
    , ccColdXvk :: Text

       -- | CIP-1852’s constitutional committee cold verification signing key (Ed25519 private key), bech32 encoded prefixed with 'cc_cold_vk'
    , ccColdVk :: Text

       -- | Constitutional committee cold verification key hash (cold credential) (blake2b_224 digest of a consitutional committee cold verification key), bech32 encoded prefixed with 'cc_cold'
    , cc_cold :: Text

       -- | CIP-1852’s constitutional committee hot extended signing key (Ed25519-bip32 extended private key), bech32 encoded prefixed with 'cc_hot_xsk'
    , ccHotXsk :: Text

       -- | CIP-1852’s constitutional committee extended hot verification signing key (Ed25519 public key with chain code), bech32 encoded prefixed with 'cc_hot_xvk'
    , ccHotXvk :: Text

       -- | CIP-1852’s constitutional committee hot verification signing key (Ed25519 private key), bech32 encoded prefixed with 'cc_hot_vk'
    , ccHotVk :: Text

       -- | Constitutional committee hot verification key hash (cold credential) (blake2b_224 digest of a consitutional committee hot verification key), bech32 encoded prefixed with 'cc_hot'
    , cc_hot :: Text
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
        let drepXPubTxt = bech32With CIP5.drep_xvk $ getPublicKeyAddr $ toXPub <$> drepXPrv
        let drepPubTxt = bech32With CIP5.drep_vk $ getVerKey $ toXPub <$> drepXPrv
        let drepTxt = bech32With CIP5.drep $ getKeyHash $ toXPub <$> drepXPrv

        let coldXPrv = deriveCCColdPrivateKey acctXPrv
        let coldXPrvTxt = bech32With CIP5.cc_cold_xsk  $ getExtendedKeyAddr coldXPrv
        let coldXPubTxt = bech32With CIP5.cc_cold_xvk $ getPublicKeyAddr $ toXPub <$> coldXPrv
        let coldPubTxt = bech32With CIP5.cc_cold_vk $ getVerKey $ toXPub <$> coldXPrv
        let coldTxt = bech32With CIP5.cc_cold $ getKeyHash $ toXPub <$> coldXPrv

        let hotXPrv = deriveCCHotPrivateKey acctXPrv
        let hotXPrvTxt = bech32With CIP5.cc_hot_xsk  $ getExtendedKeyAddr hotXPrv
        let hotXPubTxt = bech32With CIP5.cc_hot_xvk $ getPublicKeyAddr $ toXPub <$> hotXPrv
        let hotPubTxt = bech32With CIP5.cc_hot_vk $ getVerKey $ toXPub <$> hotXPrv
        let hotTxt = bech32With CIP5.cc_hot $ getKeyHash $ toXPub <$> hotXPrv

        let derivedKeysHashes = KeysHashes
                { drepXsk = drepXPrvTxt
                , drepXvk = drepXPubTxt
                , drepVk = drepPubTxt
                , drep = drepTxt
                , ccColdXsk = coldXPrvTxt
                , ccColdXvk = coldXPubTxt
                , ccColdVk = coldPubTxt
                , cc_cold = coldTxt
                , ccHotXsk = hotXPrvTxt
                , ccHotXvk = hotXPubTxt
                , ccHotVk = hotPubTxt
                , cc_hot = hotTxt
                }
        derivedKeysHashes `shouldBe` expectedKeysHashes
  where
    getVerKey = unsafeMkAddress . pubToBytes . xpubToPub . getKey
    getKeyHash = unsafeMkAddress . hashCredential . pubToBytes . xpubToPub . getKey

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
        goldenTextLazy ("addresses_" <> T.intercalate "_" mnemonic)
            (pShowOpt defaultOutputOptionsNoColor vec)
    it "should inspect correctly" $ do
        goldenByteStringLazy ("inspects" <> T.intercalate "_" mnemonic) $
            goldenEncodeJSON $ toInspect <$> inspectVec
  where
    getPaymentAddr addrKPrv net =  bech32 $ paymentAddress net (PaymentFromExtendedKey (toXPub <$> addrKPrv))
    getPointerAddr addrKPrv ptr net =  bech32 $ pointerAddress net (PaymentFromExtendedKey (toXPub <$> addrKPrv)) ptr
    getDelegationAddr addrKPrv stakeKPub net =
        bech32 $ delegationAddress net (PaymentFromExtendedKey (toXPub <$> addrKPrv)) stakeKPub
    toInspect :: Text -> Value
    toInspect = fromMaybe (String "couldn't inspect") .
        (Shelley.inspectAddress Nothing <=< fromBech32)

getExtendedKeyAddr :: Shelley depth XPrv -> Address
getExtendedKeyAddr = unsafeMkAddress . xprvToBytes . getKey

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

b16encode :: Text -> ByteString
b16encode = encode EBase16 . T.encodeUtf8

b16decode :: Text -> Either String ByteString
b16decode = fromBase16 . T.encodeUtf8
