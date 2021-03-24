{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
    , SoftDerivation (..)
    , XPrv
    , XPub
    , indexFromWord32
    , toXPub
    , unsafeMkIndex
    , xprvToBytes
    , xpubFromBytes
    , xpubToBytes
    )
import Cardano.Address.Style.Shelley
    ( Credential (..)
    , Role (..)
    , Shelley (..)
    , delegationAddress
    , deriveDelegationPrivateKey
    , deriveMultisigForDelegationPrivateKey
    , deriveMultisigForDelegationPublicKey
    , deriveMultisigForPaymentPrivateKey
    , deriveMultisigForPaymentPublicKey
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
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( rights )
import Data.Function
    ( (&) )
import Data.Maybe
    ( isNothing )
import Data.Text
    ( Text )
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
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

spec :: Spec
spec = do
    describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i) for any key" $
            property prop_publicChildKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i) for multisig for payment credential" $
            property prop_publicMultisigForPaymentDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i) for multisig for delegation credential" $
            property prop_publicMultisigForDelegationDerivation

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
                    "408a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4\
                    \81000203"
            }
        goldenTestPointerAddress GoldenTestPointerAddress
            {  verKey = "1a2a3a4a5a6a7a8a"
            ,  stakePtr = ChainPointer 128 2 3
            ,  netTag = 1
            ,  expectedAddr =
                    "418a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4\
                    \81000203"
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
        goldenTestBaseAddress GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 0
            ,  expectedAddr =
                    "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4\
                    \08b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddress GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 1
            ,  expectedAddr =
                    "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4\
                    \08b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
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

prop_publicMultisigForPaymentDerivation
    :: (SomeMnemonic, SndFactor)
    -> Index 'Soft 'PaymentK
    -> Property
prop_publicMultisigForPaymentDerivation (mw, (SndFactor sndFactor)) ix =
    multisigXPub1 === multisigXPub2
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv minBound
    multisigXPub1 = toXPub <$> deriveMultisigForPaymentPrivateKey accXPrv ix
    multisigXPub2 = deriveMultisigForPaymentPublicKey (toXPub <$> accXPrv) ix

prop_publicMultisigForDelegationDerivation
    :: (SomeMnemonic, SndFactor)
    -> Index 'Soft 'PaymentK
    -> Property
prop_publicMultisigForDelegationDerivation (mw, (SndFactor sndFactor)) ix =
    multisigXPub1 === multisigXPub2
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv minBound
    multisigXPub1 = toXPub <$> deriveMultisigForDelegationPrivateKey accXPrv ix
    multisigXPub2 = deriveMultisigForDelegationPublicKey (toXPub <$> accXPrv) ix

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
    pointerAddr = pointerAddress net (PaymentFromKey addrXPub) ptr


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
        let ptrAddr = pointerAddress tag (PaymentFromKey addrXPub) stakePtr
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
        let enterpriseAddr = Shelley.paymentAddress tag (PaymentFromKey addrXPub)
        let (Right bytes) = b16decode expectedAddr
        enterpriseAddr `shouldBe` unsafeMkAddress bytes

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

goldenTestBaseAddress :: GoldenTestBaseAddress -> SpecWith ()
goldenTestBaseAddress GoldenTestBaseAddress{..} =
    it ("base address for networkId " <> show netTag) $ do
        let (Just xPub1) =
                xpubFromBytes $ b16encode $ T.append verKeyPayment verKeyPayment
        let addrXPub = liftXPub xPub1 :: Shelley 'PaymentK XPub
        let (Just xPub2) =
                xpubFromBytes $ b16encode $ T.append verKeyStake verKeyStake
        let stakeXPub = liftXPub xPub2 :: Shelley 'DelegationK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let baseAddr = delegationAddress tag (PaymentFromKey addrXPub) (DelegationFromKey stakeXPub)
        let (Right bytes) = b16decode expectedAddr
        baseAddr `shouldBe` unsafeMkAddress bytes

data TestVector = TestVector
    {
      -- | The extended root private key, bech32 encoded prefixed with 'root_xprv'
      rootXPrv :: Text

      -- | The extended 0th account private key, bech32 encoded prefixed with 'acct_xprv'
    , accXPrv0 :: Text

      -- | The extended 1st account private key, bech32 encoded prefixed with 'acct_xprv'
    , accXPrv1 :: Text

      -- | The extended 0th address private key, bech32 encoded prefixed with 'addr_xprv'
    , addrXPrv0 :: Text

      -- | The extended 0th address public key, bech32 encoded prefixed with 'addr_xpub'
    , addrXPub0 :: Text

      -- | The extended 1st address private key, bech32 encoded prefixed with 'addr_xprv'
    , addrXPrv1 :: Text

      -- | The extended 1st address public key, bech32 encoded prefixed with 'addr_xpub'
    , addrXPub1 :: Text

      -- | The extended 1442nd address private key, bech32 encoded prefixed with 'addr_xprv'
    , addrXPrv1442 :: Text

      -- | The extended 1442nd address public key, bech32 encoded prefixed with 'addr_xpub'
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

testVectors :: [Text] -> SpecWith ()
testVectors mnemonic = it (show $ T.unpack <$> mnemonic) $ do
    let (Right mw) = mkSomeMnemonic @'[9,12,15,18,21,24] mnemonic
    let sndFactor = mempty
    let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    let (Right hrpRoot) = Bech32.humanReadablePartFromText "root_xprv"
    let rootXPrv = bech32With hrpRoot $ getExtendedKeyAddr rootK

    let (Right hrp) = Bech32.humanReadablePartFromText "acct_xprv"
    let Just accIx0 = indexFromWord32 @(Index 'Hardened _) 0x80000000
    let acctK0 = deriveAccountPrivateKey rootK accIx0
    let accXPrv0 = bech32With hrp $ getExtendedKeyAddr acctK0
    let Just accIx1 = indexFromWord32 @(Index 'Hardened _) 0x80000001
    let acctK1 = deriveAccountPrivateKey rootK accIx1
    let accXPrv1 = bech32With hrp $ getExtendedKeyAddr acctK1

    let (Right hrpPrv) = Bech32.humanReadablePartFromText "addr_xprv"
    let (Right hrpPub) = Bech32.humanReadablePartFromText "addr_xpub"
    let Just addIx0 = indexFromWord32 @(Index 'Soft _) 0x00000000
    let addrK0prv = deriveAddressPrivateKey acctK0 UTxOExternal addIx0
    let addrXPrv0 = bech32With hrpPrv $ getExtendedKeyAddr addrK0prv
    let addrXPub0 = bech32With hrpPub $ getPublicKeyAddr $ toXPub <$> addrK0prv

    let Just addIx1 = indexFromWord32 @(Index 'Soft _) 0x00000001
    let addrK1prv = deriveAddressPrivateKey acctK0 UTxOExternal addIx1
    let addrXPrv1 = bech32With hrpPrv $ getExtendedKeyAddr addrK1prv
    let addrXPub1 = bech32With hrpPub $ getPublicKeyAddr $ toXPub <$> addrK1prv
    let Just addIx1442 = indexFromWord32 @(Index 'Soft _) 0x000005a2
    let addrK1442prv = deriveAddressPrivateKey acctK0 UTxOExternal addIx1442
    let addrXPrv1442 = bech32With hrpPrv $ getExtendedKeyAddr addrK1442prv
    let addrXPub1442 = bech32With hrpPub $ getPublicKeyAddr $ toXPub <$> addrK1442prv

    let networkTags = rights $ mkNetworkDiscriminant <$> [0,3,6]
    let paymentAddr0 = getPaymentAddr addrK0prv <$> networkTags
    let paymentAddr1 = getPaymentAddr addrK1prv <$> networkTags
    let paymentAddr1442 = getPaymentAddr addrK1442prv <$> networkTags

    let slot1 = ChainPointer 1 2 3
    let pointerAddr0Slot1 = getPointerAddr addrK0prv slot1 <$> networkTags
    let slot2 = ChainPointer 24157 177 42
    let pointerAddr0Slot2 = getPointerAddr addrK0prv slot2 <$> networkTags

    let stakeKPub0 = toXPub <$> deriveDelegationPrivateKey acctK0
    let delegationAddr0Stake0 = getDelegationAddr addrK0prv (DelegationFromKey stakeKPub0) <$> networkTags
    let delegationAddr1Stake0 = getDelegationAddr addrK1prv (DelegationFromKey stakeKPub0) <$> networkTags
    let delegationAddr1442Stake0 = getDelegationAddr addrK1442prv (DelegationFromKey stakeKPub0) <$> networkTags
    let stakeKPub1 = toXPub <$> deriveDelegationPrivateKey acctK1
    let delegationAddr0Stake1 = getDelegationAddr addrK0prv (DelegationFromKey stakeKPub1) <$> networkTags
    let delegationAddr1Stake1 = getDelegationAddr addrK1prv (DelegationFromKey stakeKPub1) <$> networkTags
    let delegationAddr1442Stake1 = getDelegationAddr addrK1442prv (DelegationFromKey stakeKPub1) <$> networkTags
    let vec = TestVector {..}
    goldenTextLazy (T.intercalate "_" mnemonic) (pShowOpt defaultOutputOptionsNoColor vec)
  where
    getExtendedKeyAddr = unsafeMkAddress . xprvToBytes . getKey
    getPublicKeyAddr = unsafeMkAddress . xpubToBytes . getKey
    getPaymentAddr addrKPrv net =  bech32 $ paymentAddress net (PaymentFromKey (toXPub <$> addrKPrv))
    getPointerAddr addrKPrv ptr net =  bech32 $ pointerAddress net (PaymentFromKey (toXPub <$> addrKPrv)) ptr
    getDelegationAddr addrKPrv stakeKPub net =
        bech32 $ delegationAddress net (PaymentFromKey (toXPub <$> addrKPrv)) stakeKPub

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
    address = paymentAddress discrimination (PaymentFromKey addXPub)
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
    address = delegationAddress discrimination (PaymentFromKey addXPub) (DelegationFromKey delegXPub)
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
    address = pointerAddress discrimination (PaymentFromKey addXPub) ptr
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
