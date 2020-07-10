{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Cardano.Address.Style.ByronSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index
    , XPrv
    , xprvFromBytes
    )
import Cardano.Address.Style.Byron
    ( Byron (..), minSeedLengthBytes )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
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
import Test.Arbitrary
    ( unsafeMkMnemonic )
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
    -> Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
    -> Property
prop_keyDerivationFromSeed mw accIx addrIx =
    addrXPrv `seq` property ()
  where
    rootXPrv = genMasterKeyFromMnemonic mw () :: Byron 'RootK XPrv
    accXPrv = deriveAccountPrivateKey rootXPrv accIx
    addrXPrv = deriveAddressPrivateKey accXPrv () addrIx

prop_keyDerivationFromXPrv
    :: XPrv
    -> Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
    -> Property
prop_keyDerivationFromXPrv xprv accIx addrIx =
    addrXPrv `seq` property ()
  where
    rootXPrv = genMasterKeyFromXPrv xprv :: Byron 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv accIx
    addrXPrv = deriveAddressPrivateKey accXPrv () addrIx

{-------------------------------------------------------------------------------
                                  Golden tests
-------------------------------------------------------------------------------}

goldenSpec :: Spec
goldenSpec = describe "Golden tests" $ do
    it "unsafeGenerateKeyFromSeed - no passphrase" $
        generateTest generateTest1

{-------------------------------------------------------------------------------
                      Golden tests for generateKeyFromSeed
-------------------------------------------------------------------------------}

data GenerateKeyFromSeed = GenerateKeyFromSeed
    { mnem :: [Text]
    , rootKey :: Byron 'RootK XPrv
    }

generateTest :: GenerateKeyFromSeed -> Expectation
generateTest (GenerateKeyFromSeed mnemonic rootXPrv)  =
    getKey masterKey `shouldBe` getKey rootXPrv
  where
    mw = SomeMnemonic $ unsafeMkMnemonic @12 mnemonic
    masterKey = genMasterKeyFromMnemonic mw () :: Byron 'RootK XPrv

generateTest1 :: GenerateKeyFromSeed
generateTest1 = GenerateKeyFromSeed
    { mnem = defMnemonic
    , rootKey = xprv16
        "b84d0b6db447911a98a3ade98145c0e8323e106f07bc17a99c2104c2688bb752\
        \8310902a3cec7e262ded6a4369ec1f48966a6b48b1ee90aa00e61b95417949f8\
        \a4762129a6c83acfda5b257eaeb73ec5fee1518b6674fdc7891fe23f06174421"
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
xprv16 :: ByteString -> Byron 'RootK XPrv
xprv16 hex = genMasterKeyFromXPrv prv
  where
    Just prv = (xprvFromBytes <=< fromHexText) hex
    fromHexText :: ByteString -> Maybe ByteString
    fromHexText = either (const Nothing) Just . convertFromBase Base16

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
