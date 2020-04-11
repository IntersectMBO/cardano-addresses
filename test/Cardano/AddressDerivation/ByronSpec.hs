{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.AddressDerivation.ByronSpec
    ( spec
    ) where

import Prelude

import Cardano.AddressDerivation
    ( Depth (..), DerivationType (..), Index (..) )
import Cardano.AddressDerivation.Byron
    ( ByronKey (..)
    , generateKeyFromSeed
    , minSeedLengthBytes
    , unsafeGenerateKeyFromSeed
    , unsafeMkByronKeyFromMasterKey
    )
import Cardano.AddressDerivationSpec
    ()
import Cardano.Crypto.Wallet
    ( XPrv, unXPrv, xprv )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , Mnemonic (..)
    , SomeMnemonic (..)
    , mkMnemonic
    )
import Control.Monad
    ( (<=<) )
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import GHC.Stack
    ( HasCallStack )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), Property, choose, property, vector )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    goldenSpec
    describe "Random Address Derivation Properties" $ do
        it "Key derivation from seed works for various indexes" $
            property prop_keyDerivationFromSeed
        it "Key derivation from master key works for various indexes" $
            property prop_keyDerivationFromXPrv

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_keyDerivationFromSeed
    :: SomeMnemonic
    -> Passphrase
    -> Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
    -> Property
prop_keyDerivationFromSeed seed (Passphrase encPwd) accIx addrIx =
    rndKey `seq` property () -- NOTE Making sure this doesn't throw
  where
    rndKey :: ByronKey 'AddressK XPrv
    rndKey = unsafeGenerateKeyFromSeed (accIx, addrIx) seed encPwd

prop_keyDerivationFromXPrv
    :: XPrv
    -> Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
    -> Property
prop_keyDerivationFromXPrv masterkey accIx addrIx =
    rndKey `seq` property () -- NOTE Making sure this doesn't throw
  where
    rndKey :: ByronKey 'AddressK XPrv
    rndKey = unsafeMkByronKeyFromMasterKey (accIx, addrIx) masterkey

{-------------------------------------------------------------------------------
                                  Golden tests
-------------------------------------------------------------------------------}

goldenSpec :: Spec
goldenSpec = describe "Golden tests" $ do
    it "generateKeyFromSeed - no passphrase" $
        generateTest generateTest1

    it "generateKeyFromSeed - with passphrase" $
        generateTest generateTest2

{-------------------------------------------------------------------------------
                      Golden tests for generateKeyFromSeed
-------------------------------------------------------------------------------}

data GenerateKeyFromSeed = GenerateKeyFromSeed
    { mnem :: [Text]
    , pwd :: Passphrase
    , rootKey :: ByronKey 'RootK XPrv
    }

generateTest :: GenerateKeyFromSeed -> Expectation
generateTest (GenerateKeyFromSeed mnemonic (Passphrase pwdEnc) rootXPrv)  =
    getKey (generateKeyFromSeed mw pwdEnc)
    `shouldBe`
    getKey rootXPrv
  where
    mw = SomeMnemonic $ unsafeMkMnemonic @12 mnemonic

generateTest1 :: GenerateKeyFromSeed
generateTest1 = GenerateKeyFromSeed
    { mnem = defMnemonic
    , pwd = pp ""
    , rootKey = xprv16
        "b84d0b6db447911a98a3ade98145c0e8323e106f07bc17a99c2104c2688bb752831090\
        \2a3cec7e262ded6a4369ec1f48966a6b48b1ee90aa00e61b95417949f81258854ab44b\
        \0cfda59bd68fbd87f280841a390068049df0f8a903c94ba65b7aa4762129a6c83acfda\
        \5b257eaeb73ec5fee1518b6674fdc7891fe23f06174421"
    }

generateTest2 :: GenerateKeyFromSeed
generateTest2 = GenerateKeyFromSeed
    { mnem = defMnemonic
    , pwd = pp "4a87f05fe25a57c96ff5221863e61b91bcca566b853b616f55e5d2c18caa1a4c"
    , rootKey = xprv16
        "b842ae13cbb31b7d96910472bbed5c8729c764d66af81b48120a6a583eae55faf78c247\
        \65e0c9826f4d095f3e6addb4bda68df322b220d3c08b8a5b414232d101258854ab44b0c\
        \fda59bd68fbd87f280841a390068049df0f8a903c94ba65b7aa4762129a6c83acfda5b2\
        \57eaeb73ec5fee1518b6674fdc7891fe23f06174421"
    }

-- | This is the mnemonic that provides the 'Default' instance in cardano-sl
defMnemonic :: [Text]
defMnemonic =
    [ "squirrel", "material", "silly", "twice", "direct", "slush"
    , "pistol", "razor", "become", "junk", "kingdom", "flee" ]

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

-- | Get a private key from a hex string, without error checking.
xprv16 :: ByteString -> ByronKey 'RootK XPrv
xprv16 hex = ByronKey k () (error "passphrase not used for tests")
  where
    Right k = xprvFromText hex
    xprvFromText = xprv <=< fromHexText
    fromHexText :: ByteString -> Either String ByteString
    fromHexText = convertFromBase Base16

-- | Get a passphrase from a hex string, without error checking
pp :: ByteString -> Passphrase
pp hex = Passphrase b
    where Right b = convertFromBase Base16 hex

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

newtype Passphrase = Passphrase ScrubbedBytes
    deriving stock (Eq, Show)
    deriving newtype (ByteArrayAccess)

-- This generator will only produce valid (@>= minSeedLengthBytes@) passphrases
-- because 'generateKeyFromSeed' is a partial function.
instance {-# OVERLAPS #-} Arbitrary Passphrase  where
    arbitrary = do
        n <- choose (minSeedLengthBytes, 64)
        bytes <- BS.pack <$> vector n
        return $ Passphrase $ BA.convert bytes

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . unXPrv

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    a == b = unXPrv a == unXPrv b

instance Arbitrary XPrv where
    arbitrary = unsafeXPrv . BS.pack <$> vector 128

-- | Build a 'XPrv' from a bytestring
unsafeXPrv :: HasCallStack => ByteString -> XPrv
unsafeXPrv bytes =
    case xprv bytes of
        Left e -> error $ "unsafeXPrv: " <> e
        Right a -> a

-- | Build 'Mnemonic' from literals
unsafeMkMnemonic
    :: forall mw n csz
    .  (ConsistentEntropy n mw csz, EntropySize mw ~ n, HasCallStack)
    => [Text]
    -> Mnemonic mw
unsafeMkMnemonic m =
    case mkMnemonic m of
        Left e -> error $ "unsafeMnemonic: " <> show e
        Right a -> a
