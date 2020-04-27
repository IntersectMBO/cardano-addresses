{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Address.Style.ShelleySpec
    ( spec
    ) where

import Prelude

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
    )
import Cardano.Address.Style.Shelley
    ( Shelley (..) )
import Cardano.Mnemonic
    ( SomeMnemonic, someMnemonicToBytes )
import Data.ByteArray
    ( ScrubbedBytes )
import Test.Arbitrary
    ()
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, expectFailure, property, (===), (==>) )

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

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

prop_publicChildKeyDerivation
    :: (SomeMnemonic, Maybe SomeMnemonic)
    -> AccountingStyle
    -> Index 'Soft 'AddressK
    -> Property
prop_publicChildKeyDerivation (mw, sndFactor) cc ix =
    addrXPub1 === addrXPub2
  where
    rootXPrv =
        genMasterKeyFromMnemonic mw (maybeMnemonicToBytes sndFactor) :: Shelley 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv minBound
    addrXPub1 = toXPub <$> deriveAddressPrivateKey accXPrv cc ix
    addrXPub2 = deriveAddressPublicKey (toXPub <$> accXPrv) cc ix

prop_accountKeyDerivation
    :: (SomeMnemonic, Maybe SomeMnemonic)
    -> Index 'Hardened 'AccountK
    -> Property
prop_accountKeyDerivation (mw, sndFactor) ix =
    accXPrv `seq` property ()
  where
    rootXPrv =
        genMasterKeyFromMnemonic mw (maybeMnemonicToBytes sndFactor) :: Shelley 'RootK XPrv
    accXPrv = deriveAccountPrivateKey rootXPrv ix

maybeMnemonicToBytes :: Maybe SomeMnemonic -> ScrubbedBytes
maybeMnemonicToBytes = maybe mempty someMnemonicToBytes

prop_toEnumAccountingStyle :: Int -> Property
prop_toEnumAccountingStyle n =
    n > fromEnum UTxOInternal ==> expectFailure $ property $
        (toEnum n :: AccountingStyle) `seq` ()

prop_roundtripEnumAccountingStyle :: AccountingStyle -> Property
prop_roundtripEnumAccountingStyle ix =
    (toEnum . fromEnum) ix === ix
