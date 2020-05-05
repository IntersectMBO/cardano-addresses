{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Address.Style.ShelleySpec
    ( spec
    ) where

import Prelude

import Cardano.Address
    ( hex )
import Cardano.Address.Derivation
    ( AccountingStyle (..)
    , Depth (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index
    , SoftDerivation (..)
    , XPrv
    , toXPub
    , xprvToBytes
    )
import Cardano.Address.Style.Shelley
    ( Shelley (..) )
import Cardano.Mnemonic
    ( SomeMnemonic, mkSomeMnemonic )
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Data.Text
    ( Text )
import Test.Arbitrary
    ()
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , expectFailure
    , property
    , vector
    , (===)
    , (==>)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_publicChildKeyDerivation

    describe "Bounded / Enum relationship" $ do
        it "Calling toEnum for invalid value gives a runtime err (AccountingStyle)"
            (property prop_toEnumAccountingStyle)

    describe "Enum Roundtrip" $ do
        it "AccountingStyle" (property prop_roundtripEnumAccountingStyle)

    describe "Golden tests" $ do
        goldenTest TestVector
            { rootXPrv = "root_xprv982a5cfec6be5656be247f0060d13d3bd52403bcca62\
                         \7f1d9988ca665907c5400980a71f60e32c6efa5de59bbedd4add1\
                         \8f57955f7fe0ca434f8b6ddb48ec72202545a3943a396ccdcb0fb\
                         \98fffd989635fd93406b04f556ea54034012b72008"
            , mnemonic = [ "test", "child", "burst", "immense", "armed", "parrot"
                         , "company", "walk", "dog" ]
            }
        goldenTest TestVector
            { rootXPrv = "root_xprv608621fb4c0101feb31f6f2fd7018bee54101ff67d55\
                         \5079671893225ee1a45e2331497029d885b5634405f350508cd95\
                         \dce3991503b10f128d04f34b7b625783a1e3bd5dcf11fd4f989ec\
                         \2cdcdea3a54db8997398174ecdcc87006c274176a0"
            , mnemonic = [ "test", "walk", "nut", "penalty", "hip", "pave", "soap",
                           "entry", "language", "right", "filter", "choice" ]
            }
        goldenTest TestVector
            { rootXPrv = "root_xprvb8f2bece9bdfe2b0282f5bad705562ac996efb6af96b\
                         \648f4445ec44f47ad95c10e3d72f26ed075422a36ed8585c745a0\
                         \e1150bcceba2357d058636991f38a3791e248de509c070d812ab2\
                         \fda57860ac876bc489192c1ef4ce253c197ee219a4"
            , mnemonic = [ "art", "forum", "devote", "street", "sure", "rather",
                           "head", "chuckle", "guard", "poverty", "release",
                           "quote", "oak", "craft", "enemy"]
            }
        goldenTest TestVector
            { rootXPrv = "root_xprve8aa0904f407272f5367b0dac6911b66b95c64123b26\
                         \eea627725ba29316875e972fd37e121a09dbbacbc7c8ad118d34c\
                         \212ddaf564e2d7198d6c3f9d6d4cb6ce0387dd6b5c0393c08c239\
                         \cd9b1eaf3578d23d0c32edb0bc4d41a57011c6d2d9"
            , mnemonic = [ "churn", "shaft", "spoon", "second", "erode", "useless",
                           "thrive", "burst", "group", "seed", "element", "sign",
                           "scrub", "buffalo", "jelly", "grace", "neck", "useless" ]
            }
        goldenTest TestVector
            { rootXPrv = "root_xprvf080e8d14493d50c91612f43c9e714a794ab16381ac3\
                         \f01f1740537f60d18b515628d17049769717bdad315c2312f9b19\
                         \20bb053fa4c97267807a26e4fb4389785f9411155f742692668d3\
                         \3f263a77108908749911a3286941d5b3989a093666"
            , mnemonic = [ "draft", "ability", "female", "child", "jump", "maid",
                           "roof", "hurt", "below", "live", "topple", "paper",
                           "exclude", "ordinary", "coach", "churn", "sunset",
                           "emerge", "blame", "ketchup", "much" ]
            }
        goldenTest TestVector
            { rootXPrv = "root_xprv989c07d00e8d3d187c1855f25468e013fb1bcad93430\
                         \fc5a298a259802118f451e846b73a627c54ab13d49f5995b8563b\
                         \32ad860c019a28b0b953209cd11bc1843548a19d62a34d1714b1a\
                         \c21903524efffaba00c4f4fcb203649661b61e2ca6"
            , mnemonic = [ "excess", "behave", "track", "soul", "table", "wear",
                           "ocean", "cash", "stay", "nature", "item", "turtle",
                           "palm", "soccer", "lunch", "horror", "start", "stumble",
                           "month", "panic", "right", "must", "lock", "dress" ]
            }

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

prop_publicChildKeyDerivation
    :: (SomeMnemonic, SndFactor)
    -> AccountingStyle
    -> Index 'Soft 'AddressK
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

prop_toEnumAccountingStyle :: Int -> Property
prop_toEnumAccountingStyle n =
    n > fromEnum UTxOInternal ==> expectFailure $ property $
        (toEnum n :: AccountingStyle) `seq` ()

prop_roundtripEnumAccountingStyle :: AccountingStyle -> Property
prop_roundtripEnumAccountingStyle ix =
    (toEnum . fromEnum) ix === ix

{-------------------------------------------------------------------------------
                             Golden tests
-------------------------------------------------------------------------------}

data TestVector = TestVector
    {
      -- | The extended root private key hex-encoded, prefixed with 'root_xprv'
      rootXPrv :: Text

      -- | Corresponding Mnemonic
    , mnemonic :: [Text]
    }

goldenTest :: TestVector -> SpecWith ()
goldenTest TestVector{..} = it (show $ T.unpack <$> mnemonic) $ do
    let (Right mw) = mkSomeMnemonic @'[9,12,15,18,21,24] mnemonic
    let sndFactor = mempty
    let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    let rootXPrv' = T.append "root_xprv" (T.decodeUtf8 $ hex $ xprvToBytes $ getKey rootK)
    rootXPrv' `shouldBe` rootXPrv

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
