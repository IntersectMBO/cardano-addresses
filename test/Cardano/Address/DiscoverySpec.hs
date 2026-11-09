{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Address.DiscoverySpec
    ( spec
    ) where

import Prelude

import Cardano.Address
    ( Address, NetworkTag (..), bech32 )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index
    , XPrv
    , XPub
    , indexFromWord32
    , indexToWord32
    , toXPub
    )
import Cardano.Address.Discovery
    ( DiscoveredAddress (..)
    , DiscoveredCredential (..)
    , DiscoveryAddressType (..)
    , DiscoveryResult (..)
    , ErrDiscovery (..)
    , discover
    )
import Cardano.Address.KeyHash
    ( GovernanceType (..), KeyRole (..), keyHashToText )
import Cardano.Address.Style.Shelley
    ( Credential (..)
    , Role (..)
    , Shelley (..)
    , delegationAddress
    , deriveAddressPublicKeyShelley
    , getKey
    , hashKey
    , liftXPub
    , paymentAddress
    )
import Cardano.Mnemonic
    ( SomeMnemonic, mkSomeMnemonic )
import Data.Either
    ( isRight )
import Data.List
    ( isPrefixOf )
import Data.Maybe
    ( fromJust )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import Test.Arbitrary
    ( unsafeFromRight )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , counterexample
    , elements
    , forAll
    , property
    , withNumTests
    , (.&&.)
    , (===)
    , (==>)
    )

import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Golden tests" $ do
        goldenDiscover "mainnet base gap 0"
            (NetworkTag 1) Base 0
            Golden
                { goldenExternalCount = 0
                , goldenInternalCount = 0
                , goldenExternalAddrs = []
                , goldenInternalAddrs = []
                , goldenStake = Just "stake_vkh1xtrj35uxrctye2egew8sqezgzwwg796ql7uw02572gedc78jpfa"
                , goldenDRep = Just "drep1y2jmg4g450lced7q9n34rq6d5vjwkm0ugx6h0894u6ur92s9txn3a"
                , goldenCCCold = Just "cc_cold1ztl0h9vka4ns45kfj7xh3ljwkd46yn9m5znzlfxd6rpdeagw6p59q"
                , goldenCCHot = Just "cc_hot1qtmd98q0w9jdxasse0m8kyn2nya7kf9qwmgx20cl5p543rcdtr4dz"
                }

        goldenDiscover "mainnet base gap 1"
            (NetworkTag 1) Base 1
            Golden
                { goldenExternalCount = 1
                , goldenInternalCount = 1
                , goldenExternalAddrs =
                    [ "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqfjkjv7" ]
                , goldenInternalAddrs =
                    [ "addr1qx4kjk0as0x7ptt54l6cnfyzejqg22cku0qhqx6al4g2xepjcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqhpmech" ]
                , goldenStake = Just "stake_vkh1xtrj35uxrctye2egew8sqezgzwwg796ql7uw02572gedc78jpfa"
                , goldenDRep = Just "drep1y2jmg4g450lced7q9n34rq6d5vjwkm0ugx6h0894u6ur92s9txn3a"
                , goldenCCCold = Just "cc_cold1ztl0h9vka4ns45kfj7xh3ljwkd46yn9m5znzlfxd6rpdeagw6p59q"
                , goldenCCHot = Just "cc_hot1qtmd98q0w9jdxasse0m8kyn2nya7kf9qwmgx20cl5p543rcdtr4dz"
                }

        goldenDiscover "mainnet enterprise gap 1"
            (NetworkTag 1) Enterprise 1
            Golden
                { goldenExternalCount = 1
                , goldenInternalCount = 1
                , goldenExternalAddrs =
                    [ "addr1vx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers66hrl8" ]
                , goldenInternalAddrs =
                    [ "addr1vx4kjk0as0x7ptt54l6cnfyzejqg22cku0qhqx6al4g2xeqryym03" ]
                , goldenStake = Just "stake_vkh1xtrj35uxrctye2egew8sqezgzwwg796ql7uw02572gedc78jpfa"
                , goldenDRep = Just "drep1y2jmg4g450lced7q9n34rq6d5vjwkm0ugx6h0894u6ur92s9txn3a"
                , goldenCCCold = Just "cc_cold1ztl0h9vka4ns45kfj7xh3ljwkd46yn9m5znzlfxd6rpdeagw6p59q"
                , goldenCCHot = Just "cc_hot1qtmd98q0w9jdxasse0m8kyn2nya7kf9qwmgx20cl5p543rcdtr4dz"
                }

        goldenDiscover "testnet base gap 1"
            (NetworkTag 0) Base 1
            Golden
                { goldenExternalCount = 1
                , goldenInternalCount = 1
                , goldenExternalAddrs =
                    [ "addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp" ]
                , goldenInternalAddrs =
                    [ "addr_test1qz4kjk0as0x7ptt54l6cnfyzejqg22cku0qhqx6al4g2xepjcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq5hxe5g" ]
                , goldenStake = Just "stake_vkh1xtrj35uxrctye2egew8sqezgzwwg796ql7uw02572gedc78jpfa"
                , goldenDRep = Just "drep1y2jmg4g450lced7q9n34rq6d5vjwkm0ugx6h0894u6ur92s9txn3a"
                , goldenCCCold = Just "cc_cold1ztl0h9vka4ns45kfj7xh3ljwkd46yn9m5znzlfxd6rpdeagw6p59q"
                , goldenCCHot = Just "cc_hot1qtmd98q0w9jdxasse0m8kyn2nya7kf9qwmgx20cl5p543rcdtr4dz"
                }

        goldenDiscover "testnet enterprise gap 1"
            (NetworkTag 0) Enterprise 1
            Golden
                { goldenExternalCount = 1
                , goldenInternalCount = 1
                , goldenExternalAddrs =
                    [ "addr_test1vz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspjrlsz" ]
                , goldenInternalAddrs =
                    [ "addr_test1vz4kjk0as0x7ptt54l6cnfyzejqg22cku0qhqx6al4g2xeqcvs8q5" ]
                , goldenStake = Just "stake_vkh1xtrj35uxrctye2egew8sqezgzwwg796ql7uw02572gedc78jpfa"
                , goldenDRep = Just "drep1y2jmg4g450lced7q9n34rq6d5vjwkm0ugx6h0894u6ur92s9txn3a"
                , goldenCCCold = Just "cc_cold1ztl0h9vka4ns45kfj7xh3ljwkd46yn9m5znzlfxd6rpdeagw6p59q"
                , goldenCCHot = Just "cc_hot1qtmd98q0w9jdxasse0m8kyn2nya7kf9qwmgx20cl5p543rcdtr4dz"
                }

    describe "Validation" $ do
        it "rejects network tags >= 16" $ do
            let res = discover (NetworkTag 16) Base 1 testAccountKey
            res `shouldBe` Left (ErrInvalidNetworkTag (NetworkTag 16))

        it "rejects gaps above the soft derivation range" $ do
            let maxGap = indexToWord32 (maxBound @(Index 'Soft 'PaymentK)) + 1
            let res = discover (NetworkTag 1) Base (maxGap + 1) testAccountKey
            res `shouldBe` Left (ErrGapTooLarge (maxGap + 1))

        it "returns empty payment lists for gap 0 but still derives credentials" $ do
            let Right result = discover (NetworkTag 1) Base 0 testAccountKey
            paymentExternal result `shouldBe` []
            paymentInternal result `shouldBe` []
            stakeCredential result `shouldSatisfy` isJustWithIndex 0
            drepCredential result `shouldSatisfy` isJustWithIndex 0
            committeeColdCredential result `shouldSatisfy` isJustWithIndex 0
            committeeHotCredential result `shouldSatisfy` isJustWithIndex 0

    describe "Properties" $ do
        prop "discovered payment addresses match manual derivation" $
            property prop_paymentAddressesMatch

        prop "discovered credentials match manual derivation" $
            property prop_credentialsMatch

        prop "valid inputs always succeed" $
            property prop_validInputsSucceed

        prop "payment lists have exactly gap entries" $
            property prop_lengthsMatchGap

        prop "increasing gap extends the prefix" $
            property prop_prefixMonotonic

        prop "discover is deterministic" $
            property prop_deterministic

        prop "invalid network tags are rejected" $
            property prop_invalidNetworkTagFails

        prop "gaps above the soft range are rejected" $
            property prop_invalidGapFails

--
-- Golden test helpers
--

data Golden = Golden
    { goldenExternalCount :: !Int
    , goldenInternalCount :: !Int
    , goldenExternalAddrs :: ![Text]
    , goldenInternalAddrs :: ![Text]
    , goldenStake :: !(Maybe Text)
    , goldenDRep :: !(Maybe Text)
    , goldenCCCold :: !(Maybe Text)
    , goldenCCHot :: !(Maybe Text)
    }

goldenDiscover
    :: Text
    -> NetworkTag
    -> DiscoveryAddressType
    -> Word32
    -> Golden
    -> Spec
goldenDiscover name net typ gap golden = it (T.unpack name) $ do
    let Right result = discover net typ gap testAccountKey
    map (bech32 . address) (paymentExternal result) `shouldBe` goldenExternalAddrs golden
    map (bech32 . address) (paymentInternal result) `shouldBe` goldenInternalAddrs golden
    length (paymentExternal result) `shouldBe` goldenExternalCount golden
    length (paymentInternal result) `shouldBe` goldenInternalCount golden
    (credText <$> stakeCredential result) `shouldBe` goldenStake golden
    (credText <$> drepCredential result) `shouldBe` goldenDRep golden
    (credText <$> committeeColdCredential result) `shouldBe` goldenCCCold golden
    (credText <$> committeeHotCredential result) `shouldBe` goldenCCHot golden
  where
    credText = flip keyHashToText CIP0129 . credential

--
-- Fixed test account
--

testAccountKey :: Shelley 'AccountK XPub
testAccountKey = toXPub <$> deriveAccountPrivateKey rootK minBound
  where
    rootK :: Shelley 'RootK XPrv
    rootK = genMasterKeyFromMnemonic testMnemonic mempty

testMnemonic :: SomeMnemonic
testMnemonic = unsafeFromRight $ mkSomeMnemonic @'[12]
    [ "test", "walk", "nut", "penalty", "hip", "pave", "soap"
    , "entry", "language", "right", "filter", "choice" ]

--
-- Property helpers
--

prop_paymentAddressesMatch :: Property
prop_paymentAddressesMatch =
    forAll arbitrary $ \(NetworkTag tag) ->
    tag < 16 ==>
    forAll (elements [Base, Enterprise]) $ \typ ->
    forAll (choose (0, 50) :: Gen Word32) $ \gap ->
    forAll arbitrary $ \(accXPub :: Shelley 'AccountK XPub) ->
        let Right result = discover (NetworkTag tag) typ gap accXPub
            ixs = if gap == 0 then [] else [0 .. gap - 1]
            expected role = map (bech32 . mkPaymentAddress (NetworkTag tag) typ accXPub role) ixs
        in counterexample "external addresses mismatch" $
            map (bech32 . address) (paymentExternal result) === expected UTxOExternal
           .&&.
           counterexample "internal addresses mismatch" (
            map (bech32 . address) (paymentInternal result) === expected UTxOInternal)

prop_credentialsMatch :: Property
prop_credentialsMatch =
    forAll arbitrary $ \(NetworkTag tag) ->
    tag < 16 ==>
    forAll arbitrary $ \(accXPub :: Shelley 'AccountK XPub) ->
        let Right result = discover (NetworkTag tag) Base 1 accXPub
            expected role keyRole =
                Just $ flip keyHashToText CIP0129 $
                    hashKey keyRole (paymentXPubAt accXPub role 0)
        in counterexample "stake credential mismatch" (
            (credText <$> stakeCredential result) === expected Stake Delegation)
           .&&.
           counterexample "drep credential mismatch" (
            (credText <$> drepCredential result) === expected DRep Representative)
           .&&.
           counterexample "committee cold credential mismatch" (
            (credText <$> committeeColdCredential result) === expected CCCold CommitteeCold)
           .&&.
           counterexample "committee hot credential mismatch" (
            (credText <$> committeeHotCredential result) === expected CCHot CommitteeHot)
  where
    credText = flip keyHashToText CIP0129 . credential

prop_validInputsSucceed :: Property
prop_validInputsSucceed =
    withNumTests 50 $
    forAll arbitrary $ \(NetworkTag tag) ->
    tag < 16 ==>
    forAll (elements [Base, Enterprise]) $ \typ ->
    forAll (choose (0, 30) :: Gen Word32) $ \gap ->
    forAll arbitrary $ \(accXPub :: Shelley 'AccountK XPub) ->
        isRight (discover (NetworkTag tag) typ gap accXPub) === True

prop_lengthsMatchGap :: Property
prop_lengthsMatchGap =
    withNumTests 50 $
    forAll arbitrary $ \(NetworkTag tag) ->
    tag < 16 ==>
    forAll (elements [Base, Enterprise]) $ \typ ->
    forAll (choose (0, 50) :: Gen Word32) $ \gap ->
    forAll arbitrary $ \(accXPub :: Shelley 'AccountK XPub) ->
        let Right result = discover (NetworkTag tag) typ gap accXPub
            expected = fromIntegral gap :: Int
        in length (paymentExternal result) === expected
           .&&. length (paymentInternal result) === expected

prop_prefixMonotonic :: Property
prop_prefixMonotonic =
    withNumTests 50 $
    forAll arbitrary $ \(NetworkTag tag) ->
    tag < 16 ==>
    forAll (elements [Base, Enterprise]) $ \typ ->
    forAll (choose (0, 25) :: Gen Word32) $ \gapSmall ->
    forAll (choose (gapSmall, 50) :: Gen Word32) $ \gapLarge ->
    forAll arbitrary $ \(accXPub :: Shelley 'AccountK XPub) ->
        let Right rSmall = discover (NetworkTag tag) typ gapSmall accXPub
            Right rLarge = discover (NetworkTag tag) typ gapLarge accXPub
            bech32s = map (bech32 . address)
            prefix small large = isPrefixOf small large === True
        in counterexample "external prefix mismatch" (
            prefix (bech32s $ paymentExternal rSmall)
                   (bech32s $ paymentExternal rLarge))
           .&&.
           counterexample "internal prefix mismatch" (
            prefix (bech32s $ paymentInternal rSmall)
                   (bech32s $ paymentInternal rLarge))

prop_deterministic :: Property
prop_deterministic =
    withNumTests 50 $
    forAll arbitrary $ \(NetworkTag tag) ->
    tag < 16 ==>
    forAll (elements [Base, Enterprise]) $ \typ ->
    forAll (choose (0, 30) :: Gen Word32) $ \gap ->
    forAll arbitrary $ \(accXPub :: Shelley 'AccountK XPub) ->
        let args = ((NetworkTag tag), typ, gap, accXPub)
        in uncurry4 discover args === uncurry4 discover args
  where
    uncurry4 f (a, b, c, d) = f a b c d

prop_invalidNetworkTagFails :: Property
prop_invalidNetworkTagFails =
    withNumTests 50 $
    forAll (choose (16, 100) :: Gen Word32) $ \tag ->
    forAll (elements [Base, Enterprise]) $ \typ ->
    forAll (choose (0, 10) :: Gen Word32) $ \gap ->
    forAll arbitrary $ \(accXPub :: Shelley 'AccountK XPub) ->
        discover (NetworkTag tag) typ gap accXPub
            === Left (ErrInvalidNetworkTag (NetworkTag tag))

prop_invalidGapFails :: Property
prop_invalidGapFails =
    withNumTests 50 $
    forAll arbitrary $ \(NetworkTag tag) ->
    tag < 16 ==>
    forAll (elements [Base, Enterprise]) $ \typ ->
    forAll (choose (maxSoftGap + 1, maxBound) :: Gen Word32) $ \gap ->
    forAll arbitrary $ \(accXPub :: Shelley 'AccountK XPub) ->
        discover (NetworkTag tag) typ gap accXPub === Left (ErrGapTooLarge gap)
  where
    maxSoftGap = indexToWord32 (maxBound @(Index 'Soft 'PaymentK)) + 1

mkPaymentAddress
    :: NetworkTag
    -> DiscoveryAddressType
    -> Shelley 'AccountK XPub
    -> Role
    -> Word32
    -> Address
mkPaymentAddress net typ accXPub role ix =
    let payCred = PaymentFromExtendedKey (paymentXPubAt accXPub role ix)
    in case typ of
        Enterprise ->
            paymentAddress net payCred
        Base ->
            delegationAddress net payCred
                (DelegationFromExtendedKey (stakeXPubAt accXPub))

paymentXPubAt :: Shelley 'AccountK XPub -> Role -> Word32 -> Shelley 'PaymentK XPub
paymentXPubAt accXPub role ix = liftXPub $ deriveXPub accXPub role ix

stakeXPubAt :: Shelley 'AccountK XPub -> Shelley 'DelegationK XPub
stakeXPubAt accXPub = liftXPub $ deriveXPub accXPub Stake 0

deriveXPub :: Shelley 'AccountK XPub -> Role -> Word32 -> XPub
deriveXPub accXPub role ix =
    deriveAddressPublicKeyShelley (getKey accXPub) role index'
  where
    index' = fromJust $ indexFromWord32 @(Index 'Soft 'PaymentK) ix

isJustWithIndex :: Word32 -> Maybe DiscoveredCredential -> Bool
isJustWithIndex expected = \case
    Just (DiscoveredCredential ix _) -> ix == expected
    Nothing -> False
