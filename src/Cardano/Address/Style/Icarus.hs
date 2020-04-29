{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0

module Cardano.Address.Style.Icarus
    ( -- $overview

      -- * Icarus
      Icarus

      -- * Accessors
    , getKey

      -- * Unsafe
    , liftXPrv

      -- Internals
    , unsafeGenerateKeyFromHardwareLedger
    , minSeedLengthBytes
    ) where

import Prelude

import Cardano.Address
    ( NetworkDiscriminant (..)
    , NetworkDiscriminantByron (..)
    , PaymentAddress (..)
    , ProtocolMagic (..)
    , unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( AccountingStyle
    , Depth (..)
    , DerivationScheme (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index
    , SoftDerivation (..)
    , XPrv
    , deriveXPrv
    , deriveXPub
    , generateNew
    , xprvFromBytes
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy, mnemonicToText )
import Control.Arrow
    ( first )
import Control.DeepSeq
    ( NFData )
import Control.Exception.Base
    ( assert )
import Crypto.Hash.Algorithms
    ( SHA256 (..), SHA512 (..) )
import Crypto.MAC.HMAC
    ( HMAC, hmac )
import Data.Bits
    ( clearBit, setBit, testBit )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Codec.Cbor as CBOR
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- $overview
--
-- This module provides an implementation of:
--
-- - 'GenMasterKey': for generating Icarus master keys from mnemonic sentences
-- - 'HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'SoftDerivation': for hierarchical soft derivation of parent to child keys
-- - 'PaymentAddress': for constructing addresses from a public key
--
-- We call 'Icarus' addresses the new format of Cardano addresses which came
-- after 'Byron'. This is the format used by /Yoroi/ and now also used by
-- /Daedalus/.
--
-- == Examples
--
-- === Generating a root key from 'SomeMnemonic'
-- > :set -XOverloadedStrings
-- > :set -XTypeApplications
-- > :set -XDataKinds
-- > import Cardano.Mnemonic ( mkSomeMnemonic )
-- > import Cardano.Address.Derivation ( GenMasterKey(..) )
-- >
-- > let (Right mw) = mkSomeMnemonic @'[15] ["network","empty","cause","mean","expire","private","finger","accident","session","problem","absurd","banner","stage","void","what"]
-- > let sndFactor = mempty -- Or alternatively, a second factor passphrase
-- > let rootK = genMasterKeyFromMnemonic mw sndFactor :: Icarus 'RootK XPrv
--
-- === Generating an 'Address' from a root key
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'/0/14@
--
--
-- > import Cardano.Address ( PaymentAddress(..), base58, mainnetDiscriminant )
-- > import Cardano.Address.Derivation ( AccountingStyle(..), GenMasterKey(..), toXPub )
-- >
-- > let accIx = toEnum 0x80000000
-- > let acctK = deriveAccountPrivateKey rootK accIx
-- >
-- > let addIx = toEnum 0x00000014
-- > let addrK = deriveAddressPrivateKey acctK UTxOExternal addIx
-- >
-- > base58 $ paymentAddress mainnetDiscriminant (toXPub <$> addrK)
-- > "Ae2tdPwUPEZ8XpsjgQPH2cJdtohkYrxJ3i5y6mVsrkZZkdpdn6mnr4Rt6wG"

{-------------------------------------------------------------------------------
                                   Key Types
-------------------------------------------------------------------------------}
-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Icarus 'RootK XPrv
-- let accountPubKey  = Icarus 'AccountK XPub
-- let addressPubKey  = Icarus 'AddressK XPub
-- @
--
-- @since 1.0.0
newtype Icarus (depth :: Depth) key = Icarus
    { getKey :: key
        -- ^ Extract the raw 'XPrv' or 'XPub' wrapped by this type.
        --
        -- @since 1.0.0
    }
    deriving stock (Generic, Show, Eq)

deriving instance (Functor (Icarus depth))
instance (NFData key) => NFData (Icarus depth key)

-- | Unsafe backdoor for constructing an 'Icarus' key from a raw 'XPrv'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 1.0.0
liftXPrv :: XPrv -> Icarus depth XPrv
liftXPrv = Icarus

instance GenMasterKey Icarus where
    type SecondFactor Icarus = ScrubbedBytes

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic (SomeMnemonic mw) sndFactor =
        let
            seed  = entropyToBytes $ mnemonicToEntropy mw
            seedValidated = assert
                (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
                seed
        in Icarus $ generateNew seedValidated sndFactor

instance HardDerivation Icarus where
    type AccountIndexDerivationType Icarus = 'Hardened
    type AddressIndexDerivationType Icarus = 'Soft
    type WithAccountStyle Icarus = AccountingStyle

    deriveAccountPrivateKey (Icarus rootXPrv) accIx =
        let
            purposeIx =
                toEnum @(Index 'Hardened _) $ fromEnum purposeIndex
            coinTypeIx =
                toEnum @(Index 'Hardened _) $ fromEnum coinTypeIndex
            purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
                deriveXPrv DerivationScheme2 rootXPrv purposeIx
            coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
                deriveXPrv DerivationScheme2 purposeXPrv coinTypeIx
            acctXPrv = -- lvl3 derivation; hardened derivation of account' index
                deriveXPrv DerivationScheme2 coinTypeXPrv accIx
        in
            Icarus acctXPrv

    deriveAddressPrivateKey (Icarus accXPrv) accountingStyle addrIx =
        let
            changeCode =
                toEnum @(Index 'Soft _) $ fromEnum accountingStyle
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv changeCode
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv addrIx
        in
            Icarus addrXPrv

instance SoftDerivation Icarus where
    deriveAddressPublicKey (Icarus accXPub) accountingStyle addrIx =
        fromMaybe errWrongIndex $ do
            let changeCode = toEnum @(Index 'Soft _) $ fromEnum accountingStyle
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub changeCode
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ Icarus addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

instance HasNetworkDiscriminant Icarus where
    type NetworkDiscriminant Icarus = NetworkDiscriminantByron
    type NetworkNumber Icarus = Word32

    requiredInAddress RequiresNoMagic = False
    requiredInAddress (RequiresMagic _) = True

    networkNumber (RequiresMagic (ProtocolMagic pm)) = pm
    networkNumber RequiresNoMagic = error "no asking for magic tag here"


instance PaymentAddress Icarus where
    paymentAddress discrimination k = unsafeMkAddress
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getKey k) attrs
      where
        attrs = if requiredInAddress @Icarus discrimination then
            [ CBOR.encodeProtocolMagicAttr (networkNumber @Icarus discrimination)
            ]
            else
            []

{-------------------------------------------------------------------------------
                                 Key generation
-------------------------------------------------------------------------------}

-- | Purpose is a constant set to 44' (or 0x8000002C) following the original
-- BIP-44 specification.
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000002C

-- | One master node (seed) can be used for unlimited number of independent
-- cryptocoins such as Bitcoin, Litecoin or Namecoin. However, sharing the
-- same space for various cryptocoins has some disadvantages.
--
-- This level creates a separate subtree for every cryptocoin, avoiding reusing
-- addresses across cryptocoins and improving privacy issues.
--
-- Coin type is a constant, set for each cryptocoin. For Cardano this constant
-- is set to 1815' (or 0x80000717). 1815 is the birthyear of our beloved Ada
-- Lovelace.
--
-- Hardened derivation is used at this level.
coinTypeIndex :: Word32
coinTypeIndex = 0x80000717

-- The minimum seed length for 'generateKeyFromMnemonic' and 'unsafeGenerateKeyFromMnemonic'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- Hardware Ledger devices generates keys from mnemonic using a different
-- approach (different from the rest of Cardano).
--
-- It is a combination of:
--
-- - [SLIP 0010](https://github.com/satoshilabs/slips/blob/master/slip-0010.md)
-- - [BIP 0032](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
-- - [BIP 0039](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
-- - [RFC 8032](https://tools.ietf.org/html/rfc8032#section-5.1.5)
-- - What seems to be arbitrary changes from Ledger regarding the calculation of
--   the initial chain code and generation of the root private key.
unsafeGenerateKeyFromHardwareLedger
    :: SomeMnemonic
        -- ^ The root mnemonic
    -> Icarus 'RootK XPrv
unsafeGenerateKeyFromHardwareLedger (SomeMnemonic mw) = unsafeFromRight $ do
    let seed = pbkdf2HmacSha512
            $ T.encodeUtf8
            $ T.intercalate " "
            $ mnemonicToText mw

    -- NOTE
    -- SLIP-0010 refers to `iR` as the chain code. Here however, the chain code
    -- is obtained as a hash of the initial seed whereas iR is used to make part
    -- of the root private key itself.
    let cc = hmacSha256 (BS.pack [1] <> seed)
    let (iL, iR) = first pruneBuffer $ hashRepeatedly seed

    prv <- maybe (Left "invalid xprv") pure $ xprvFromBytes $ iL <> iR <> cc
    pure $ Icarus prv
  where
    -- Errors yielded in the body of 'unsafeGenerateKeyFromHardwareLedger' are
    -- programmer errors (out-of-range byte buffer access or, invalid length for
    -- cryptographic operations). Therefore, we throw badly if we encounter any.
    unsafeFromRight :: Either String a -> a
    unsafeFromRight = either error id

    -- This is the algorithm described in SLIP 0010 for master key generation
    -- with an extra step to discard _some_ of the potential private keys. Why
    -- this extra step remains a mystery as of today.
    --
    --      1. Generate a seed byte sequence S of 512 bits according to BIP-0039.
    --         (done in a previous step, passed as argument).
    --
    --      2. Calculate I = HMAC-SHA512(Key = "ed25519 seed", Data = S)
    --
    --      3. Split I into two 32-byte sequences, IL and IR.
    --
    -- extra *******************************************************************
    -- *                                                                       *
    -- *    3.5 If the third highest bit of the last byte of IL is not zero    *
    -- *        S = I and go back to step 2.                                   *
    -- *                                                                       *
    -- *************************************************************************
    --
    --      4. Use parse256(IL) as master secret key, and IR as master chain code.
    hashRepeatedly :: ByteString -> (ByteString, ByteString)
    hashRepeatedly bytes = case BS.splitAt 32 (hmacSha512 bytes) of
        (iL, iR) | isInvalidKey iL -> hashRepeatedly (iL <> iR)
        (iL, iR) -> (iL, iR)
      where
        isInvalidKey k = testBit (k `BS.index` 31) 5

    -- - Clear the lowest 3 bits of the first byte
    -- - Clear the highest bit of the last byte
    -- - Set the second highest bit of the last byte
    --
    -- As described in [RFC 8032 - 5.1.5](https://tools.ietf.org/html/rfc8032#section-5.1.5)
    pruneBuffer :: ByteString -> ByteString
    pruneBuffer bytes =
        let
            (firstByte, rest) = fromMaybe (error "pruneBuffer: no first byte") $
                BS.uncons bytes

            (rest', lastByte) = fromMaybe (error "pruneBuffer: no last byte") $
                BS.unsnoc rest

            firstPruned = firstByte
                & (`clearBit` 0)
                & (`clearBit` 1)
                & (`clearBit` 2)

            lastPruned = lastByte
                & (`setBit` 6)
                & (`clearBit` 7)
        in
            (firstPruned `BS.cons` BS.snoc rest' lastPruned)

    -- As described in [BIP 0039 - From Mnemonic to Seed](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki#from-mnemonic-to-seed)
    pbkdf2HmacSha512 :: ByteString -> ByteString
    pbkdf2HmacSha512 bytes = PBKDF2.generate
        (PBKDF2.prfHMAC SHA512)
        (PBKDF2.Parameters 2048 64)
        bytes
        ("mnemonic" :: ByteString)

    hmacSha256 :: ByteString -> ByteString
    hmacSha256 =
        BA.convert @(HMAC SHA256) . hmac salt

    -- As described in [SLIP 0010 - Master Key Generation](https://github.com/satoshilabs/slips/blob/master/slip-0010.md#master-key-generation)
    hmacSha512 :: ByteString -> ByteString
    hmacSha512 =
        BA.convert @(HMAC SHA512) . hmac salt

    salt :: ByteString
    salt = "ed25519 seed"
