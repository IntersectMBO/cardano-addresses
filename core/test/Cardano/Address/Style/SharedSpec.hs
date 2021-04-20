{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Address.Style.SharedSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index (..)
    , SoftDerivation (..)
    , XPrv
    , toXPub
    )
import Cardano.Address.Style.Shared
    ( Shared (..)
    , deriveMultisigForDelegationPrivateKey
    , deriveMultisigForDelegationPublicKey
    , deriveMultisigForPaymentPrivateKey
    , deriveMultisigForPaymentPublicKey
    )
import Cardano.Address.Style.Shelley
    ( Role (..) )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Test.Arbitrary
    ()
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Property, choose, property, vector, (===) )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

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
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shared 'RootK XPrv
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
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shared 'RootK XPrv
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
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shared 'RootK XPrv
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
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shared 'RootK XPrv
    accXPrv = deriveAccountPrivateKey rootXPrv ix

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
