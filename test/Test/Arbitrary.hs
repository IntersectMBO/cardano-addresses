{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary
    (
      genMnemonic
    , unsafeMkMnemonic
    , unsafeMkSomeMnemonicFromEntropy
    , unsafeFromHex
    ) where

import Prelude

import Cardano.Address
    ( NetworkDiscriminant (..), ProtocolMagic (..) )
import Cardano.Address.Derivation
    ( AccountingStyle, Depth (..), DerivationType (..), Index )
import Cardano.Address.Style.Byron
    ( Byron )
import Cardano.Address.Style.Icarus
    ( Icarus )
import Cardano.Crypto.Wallet
    ( XPrv, XPub, unXPrv, xprv )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , Entropy
    , EntropySize
    , Mnemonic
    , MnemonicException (..)
    , MnemonicWords
    , SomeMnemonic (..)
    , entropyToMnemonic
    , mkEntropy
    , mkMnemonic
    )
import Crypto.Encoding.BIP39
    ( ValidChecksumSize, ValidEntropySize, ValidMnemonicSentence )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( on )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( natVal )
import Test.QuickCheck
    ( Arbitrary (..), Gen, arbitraryBoundedEnum, oneof, vector )

import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

--
-- Arbitrary Instances
--

-- | The initial seed has to be vector or length multiple of 4 bytes and shorter
-- than 64 bytes. Note that this is good for testing or examples, but probably
-- not for generating truly random Mnemonic words.
--
-- See 'Crypto.Random.Entropy (getEntropy)'
instance
    ( ValidEntropySize n
    , ValidChecksumSize n csz
    ) => Arbitrary (Entropy n) where
    arbitrary =
        let
            size = fromIntegral $ natVal @n Proxy
            entropy =
                mkEntropy  @n . BA.convert . B8.pack <$> vector (size `quot` 8)
        in
            either (error . show . UnexpectedEntropyError) id <$> entropy

-- | Same remark from 'Arbitrary Entropy' applies here.
instance
    ( n ~ EntropySize mw
    , mw ~ MnemonicWords n
    , ValidChecksumSize n csz
    , ValidEntropySize n
    , ValidMnemonicSentence mw
    , Arbitrary (Entropy n)
    ) => Arbitrary (Mnemonic mw) where
    arbitrary =
        entropyToMnemonic <$> arbitrary @(Entropy n)

instance Arbitrary (Index 'Soft 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'WholeDomain 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'WholeDomain 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12

instance Arbitrary XPrv where
    arbitrary = unsafeXPrv . BS.pack <$> vector 128

instance Arbitrary AccountingStyle where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Byron 'AddressK XPub) where
    shrink _ = []
    arbitrary = do
        mw <- SomeMnemonic <$> genMnemonic @12
        path <- (,) <$> arbitrary <*> arbitrary
        pure $ Byron.publicKey $ Byron.unsafeGenerateKeyFromSeed path mw mempty

instance Arbitrary (Icarus 'AddressK XPub) where
    shrink _ = []
    arbitrary = do
        mw <- SomeMnemonic <$> genMnemonic @15
        pure $ Icarus.publicKey $ Icarus.unsafeGenerateKeyFromSeed mw mempty

instance Arbitrary NetworkDiscriminant where
    arbitrary = oneof
        [ pure RequiresNoMagic
        , RequiresMagic <$> arbitrary
        ]

instance Arbitrary ProtocolMagic where
    shrink (ProtocolMagic pm) = ProtocolMagic <$> shrink pm
    arbitrary = ProtocolMagic <$> arbitrary

--
-- Extra Instances
--

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . unXPrv

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    (==) = (==) `on` unXPrv

--
-- Useful functions
--

-- | Generates an arbitrary mnemonic of a size according to the type parameter.
--
-- E.g:
-- >>> arbitrary = SomeMnemonic <$> genMnemonic @12
genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vector n
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent

unsafeMkEntropy
    :: forall ent csz.
        ( HasCallStack
        , ValidEntropySize ent
        , ValidChecksumSize ent csz
        )
    => ByteString
    -> Entropy ent
unsafeMkEntropy = either (error . show) id . mkEntropy . BA.convert

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

unsafeMkSomeMnemonicFromEntropy
    :: forall mw ent csz.
        ( HasCallStack
        , ValidEntropySize ent
        , ValidChecksumSize ent csz
        , ValidMnemonicSentence mw
        , ent ~ EntropySize mw
        , mw ~ MnemonicWords ent
        )
    => Proxy mw
    -> ByteString
    -> SomeMnemonic
unsafeMkSomeMnemonicFromEntropy _ = SomeMnemonic
    . entropyToMnemonic
    . unsafeMkEntropy @ent

-- | Build a 'XPrv' from a bytestring
unsafeXPrv :: HasCallStack => ByteString -> XPrv
unsafeXPrv bytes =
    case xprv bytes of
        Left e -> error $ "unsafeXPrv: " <> e
        Right a -> a

-- | Decode an hex-encoded 'ByteString' into raw bytes, or fail.
unsafeFromHex :: HasCallStack => ByteString -> ByteString
unsafeFromHex =
    either (error . show) id . convertFromBase @ByteString @ByteString Base16
