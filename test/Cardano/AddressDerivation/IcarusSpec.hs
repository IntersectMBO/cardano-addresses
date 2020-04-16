{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.AddressDerivation.IcarusSpec
    ( spec
    ) where

import Prelude

import Cardano.AddressDerivation
    ( AccountingStyle
    , Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , SoftDerivation (..)
    )
import Cardano.AddressDerivation.Icarus
    ( Icarus (..), minSeedLengthBytes, unsafeGenerateKeyFromSeed )
import Cardano.Crypto.Wallet
    ( XPrv, toXPub )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
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
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_publicChildKeyDerivation

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

-- | Deriving address public key should be equal to deriving address
-- private key and extracting public key from it (works only for non-hardened
-- child keys).
--
-- To compute the public child key of a parent private key:
--  * N(CKDpriv((kpar, cpar), i)) (works always).
--  * CKDpub(N(kpar, cpar), i) (works only for non-hardened child keys).
--
-- Thus:
--
-- N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)
--
-- if (kpar, cpar) is a non-hardened key.
--
-- For details see <https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#private-parent-key--public-child-key bip-0039>
prop_publicChildKeyDerivation
    :: SomeMnemonic
    -> Passphrase
    -> AccountingStyle
    -> Index 'Soft 'AddressK
    -> Property
prop_publicChildKeyDerivation seed (Passphrase encPwd) cc ix =
    addrXPub1 === addrXPub2
  where
    accXPrv = unsafeGenerateKeyFromSeed seed encPwd :: Icarus 'AccountK XPrv
    -- N(CKDpriv((kpar, cpar), i))
    addrXPub1 = publicKey $ deriveAddressPrivateKey encPwd accXPrv cc ix
    -- CKDpub(N(kpar, cpar), i)
    addrXPub2 = deriveAddressPublicKey (publicKey accXPrv) cc ix
    publicKey (Icarus k) = Icarus $ toXPub k

prop_accountKeyDerivation
    :: SomeMnemonic
    -> Passphrase
    -> Index 'Hardened 'AccountK
    -> Property
prop_accountKeyDerivation seed (Passphrase encPwd) ix =
    accXPrv `seq` property () -- NOTE Making sure this doesn't throw
  where
    rootXPrv = unsafeGenerateKeyFromSeed seed encPwd :: Icarus 'RootK XPrv
    accXPrv = deriveAccountPrivateKey encPwd rootXPrv ix

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

newtype Passphrase = Passphrase ScrubbedBytes
    deriving stock (Eq, Show)
    deriving newtype (ByteArrayAccess)

instance Arbitrary Passphrase where
    arbitrary = do
        n <- choose (minSeedLengthBytes, 64)
        bytes <- BS.pack <$> vector n
        return $ Passphrase $ BA.convert bytes
