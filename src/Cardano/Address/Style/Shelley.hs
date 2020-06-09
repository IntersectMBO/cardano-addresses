{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0

module Cardano.Address.Style.Shelley
    ( -- $overview

      -- * Shelley
      Shelley
    , getKey

      -- * Key Derivation
      -- $keyDerivation
    , genMasterKeyFromXPrv
    , genMasterKeyFromMnemonic
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveStakingPrivateKey
    , deriveAddressPublicKey

      -- * Addresses
      -- $addresses
    , inspectShelleyAddress
    , paymentAddress
    , delegationAddress
    , pointerAddress
    , extendAddress
    , ErrExtendAddress (..)

      -- * Network Discrimination
    , MkNetworkDiscriminantError (..)
    , mkNetworkDiscriminant
    , shelleyMainnet
    , shelleyTestnet

      -- * Unsafe
    , liftXPrv
    , liftXPub

      -- Internals
    , minSeedLengthBytes
    , publicKeyHashSize
    ) where

import Prelude

import Cardano.Address
    ( Address
    , AddressDiscrimination (..)
    , ChainPointer (..)
    , NetworkDiscriminant (..)
    , NetworkTag (..)
    , invariantNetworkTag
    , invariantSize
    , unAddress
    , unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( AccountingStyle
    , Depth (..)
    , DerivationScheme (..)
    , DerivationType (..)
    , Index
    , XPrv
    , XPub
    , deriveXPrv
    , deriveXPub
    , generateNew
    , xpubPublicKey
    )
import Cardano.Address.Style.Byron
    ( inspectByronAddress )
import Cardano.Address.Style.Icarus
    ( inspectIcarusAddress )
import Cardano.Mnemonic
    ( SomeMnemonic, someMnemonicToBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( first )
import Control.DeepSeq
    ( NFData )
import Control.Exception.Base
    ( assert )
import Control.Monad
    ( when )
import Control.Monad
    ( guard )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_224 (..) )
import Crypto.Hash.IO
    ( HashAlgorithm (hashDigestSize) )
import Data.Binary.Get
    ( runGetOrFail )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.Bits
    ( (.&.) )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Functor
    ( ($>) )
import Data.Maybe
    ( fromMaybe, isNothing )
import Data.Word
    ( Word32 )
import Data.Word7
    ( getVariableLengthNat, putVariableLengthNat )
import GHC.Generics
    ( Generic )

import qualified Cardano.Address as Internal
import qualified Cardano.Address.Derivation as Internal
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- $overview
--
-- This module provides an implementation of:
--
-- - 'Cardano.Address.Derivation.GenMasterKey': for generating Shelley master keys from mnemonic sentences
-- - 'Cardano.Address.Derivation.HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'Cardano.Address.Derivation.SoftDerivation': for hierarchical soft derivation of parent to child keys
-- - 'Cardano.Address.PaymentAddress': for constructing payment addresses from a address public key
-- - 'Cardano.Address.DelegationAddress': for constructing delegation addresses from address and staking public keys

-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Shelley 'RootK XPrv
-- let accountPubKey  = Shelley 'AccountK XPub
-- let addressPubKey  = Shelley 'AddressK XPub
-- @
--
-- @since 2.0.0
newtype Shelley (depth :: Depth) key = Shelley
    { getKey :: key
        -- ^ Extract the raw 'XPrv' or 'XPub' wrapped by this type.
        --
        -- @since 1.0.0
    }
    deriving stock (Generic, Show, Eq)

deriving instance (Functor (Shelley depth))
instance (NFData key) => NFData (Shelley depth key)

--
-- Key Derivation
--
-- $keyDerivation
--
-- === Generating a root key from 'SomeMnemonic'
-- > :set -XOverloadedStrings
-- > :set -XTypeApplications
-- > :set -XDataKinds
-- > import Cardano.Mnemonic ( mkSomeMnemonic )
-- >
-- > let (Right mw) = mkSomeMnemonic @'[15] ["network","empty","cause","mean","expire","private","finger","accident","session","problem","absurd","banner","stage","void","what"]
-- > let sndFactor = mempty -- Or alternatively, a second factor mnemonic transformed to bytes via someMnemonicToBytes
-- > let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
--
-- === Deriving child keys
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'\/0\/14@
--
-- > import Cardano.Address.Derivation ( AccountingStyle(..) )
-- >
-- > let accIx = toEnum 0x80000000
-- > let acctK = deriveAccountPrivateKey rootK accIx
-- >
-- > let addIx = toEnum 0x00000014
-- > let addrK = deriveAddressPrivateKey acctK UTxOExternal addIx
--
-- > let stakeK = deriveStakingPrivateKey acctK

instance Internal.GenMasterKey Shelley where
    type SecondFactor Shelley = ScrubbedBytes

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic fstFactor sndFactor =
        Shelley $ generateNew seedValidated sndFactor
        where
            seed  = someMnemonicToBytes fstFactor
            seedValidated = assert
                (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
                seed

instance Internal.HardDerivation Shelley where
    type AccountIndexDerivationType Shelley = 'Hardened
    type AddressIndexDerivationType Shelley = 'Soft
    type WithAccountStyle Shelley = AccountingStyle

    deriveAccountPrivateKey (Shelley rootXPrv) accIx =
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
            Shelley acctXPrv

    deriveAddressPrivateKey (Shelley accXPrv) accountingStyle addrIx =
        let
            changeCode =
                toEnum @(Index 'Soft _) $ fromEnum accountingStyle
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv changeCode
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv addrIx
        in
            Shelley addrXPrv

instance Internal.SoftDerivation Shelley where
    deriveAddressPublicKey (Shelley accXPub) accountingStyle addrIx =
        fromMaybe errWrongIndex $ do
            let changeCode = toEnum @(Index 'Soft _) $ fromEnum accountingStyle
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub changeCode
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ Shelley addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

instance Internal.StakingDerivation Shelley where
    deriveStakingPrivateKey (Shelley accXPrv) =
        let
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv (toEnum @(Index 'Soft _) 2)
            stakeXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv (minBound @(Index 'Soft _))
        in
            Shelley stakeXPrv

-- | Generate a root key from a corresponding mnemonic.
--
-- @since 2.0.0
genMasterKeyFromMnemonic
    :: SomeMnemonic
        -- ^ Some valid mnemonic sentence.
    -> ScrubbedBytes
        -- ^ An optional second-factor passphrase (or 'mempty')
    -> Shelley 'RootK XPrv
genMasterKeyFromMnemonic =
    Internal.genMasterKeyFromMnemonic

-- | Generate a root key from a corresponding root 'XPrv'
--
-- @since 2.0.0
genMasterKeyFromXPrv
    :: XPrv -> Shelley 'RootK XPrv
genMasterKeyFromXPrv =
    Internal.genMasterKeyFromXPrv

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an account private key from the given root private key.
--
-- @since 2.0.0
deriveAccountPrivateKey
    :: Shelley 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> Shelley 'AccountK XPrv
deriveAccountPrivateKey =
    Internal.deriveAccountPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an address private key from the given account private key.
--
-- @since 2.0.0
deriveAddressPrivateKey
    :: Shelley 'AccountK XPrv
    -> AccountingStyle
    -> Index 'Soft 'AddressK
    -> Shelley 'AddressK XPrv
deriveAddressPrivateKey =
    Internal.deriveAddressPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derives an address public key from the given account public key.
--
-- @since 2.0.0
deriveAddressPublicKey
    :: Shelley 'AccountK XPub
    -> AccountingStyle
    -> Index 'Soft 'AddressK
    -> Shelley 'AddressK XPub
deriveAddressPublicKey =
    Internal.deriveAddressPublicKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derive a staking key for a corresponding 'AccountK'. Note that wallet
-- software are by convention only using one staking key per account, and always
-- the first account (with index 0').
--
-- Deriving staking keys for something else than the initial account is not
-- recommended and can lead to incompatibility with existing wallet softwares
-- (Daedalus, Yoroi, Adalite...).
--
-- @since 2.0.0
deriveStakingPrivateKey
    :: Shelley 'AccountK XPrv
    -> Shelley 'StakingK XPrv
deriveStakingPrivateKey =
    Internal.deriveStakingPrivateKey

--
-- Addresses
--
-- $addresses
-- === Generating a 'PaymentAddress'
--
-- > import Cardano.Address ( bech32 )
-- > import Cardano.Address.Derivation ( AccountingStyle(..), toXPub )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > bech32 $ paymentAddress tag (toXPub <$> addrK)
-- > "addr1vxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdncxsce5t"
--
-- === Generating a 'DelegationAddress'
--
-- > import Cardano.Address ( DelegationAddress (..) )
-- > import Cardano.Address.Derivation ( StakingDerivation (..) )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > bech32 $ delegationAddress tag (toXPub <$> addrK) (toXPub <$> stakeK)
-- > "addr1qxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdn7nudck0fzve4346yytz3wpwv9yhlxt7jwuc7ytwx2vfkyqmkc5xa"
--
-- === Generating a 'PointerAddress'
--
-- > import Cardano.Address ( PointerAddress (..), ChainPointer (..) )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > let ptr = ChainPointer 123 1 2
-- > bech32 $ pointerAddress tag (toXPub <$> addrK) ptr
-- > "addr1gxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdnmmqypqfcp5um"

instance Internal.PaymentAddress Shelley where
    paymentAddress discrimination k = unsafeMkAddress $
        invariantSize expectedLength $ BL.toStrict $ runPut $ do
            putWord8 firstByte
            putByteString (blake2b224 k)
      where
          -- we use here the fact that payment address stands for what is named
          -- as enterprise address, ie., address carrying no stake rights. For
          -- rationale why we may need such addresses refer to delegation
          -- specification - Section 3.2.3. What is important here is that the
          -- address is composed of discrimination byte and 28 bytes hashed public key.
          -- Moreover, it was decided that first 4 bits for enterprise address
          -- will be `0110`. The next for 4 bits are reserved for network discriminator.
          -- `0110 0000` is 96 in decimal.
          firstByte =
              96 + invariantNetworkTag 16 (networkTag @Shelley discrimination)
          expectedLength = 1 + publicKeyHashSize

instance Internal.DelegationAddress Shelley where
    delegationAddress discrimination paymentKey =
        unsafeFromRight
        . extendAddress (paymentAddress discrimination paymentKey)
        . Left
      where
        unsafeFromRight = either
            (error "impossible: interally generated invalid address")
            id

instance Internal.PointerAddress Shelley where
    pointerAddress discrimination paymentKey =
        unsafeFromRight
        . extendAddress (paymentAddress discrimination paymentKey)
        . Right
      where
        unsafeFromRight = either
            (error "impossible: interally generated invalid address")
            id

-- | Analyze an 'Address' to know whether it's a Shelley address or not.
--
-- Returns 'Nothing' if it's not a valid Shelley address, or a ready-to-print
-- string giving details about the 'Address'.
--
-- @since 2.0.0
inspectShelleyAddress :: Address -> Maybe String
inspectShelleyAddress addr
    | BS.length bytes < 1 + publicKeyHashSize = Nothing
    | otherwise =
        let
            (fstByte, rest) = first BS.head $ BS.splitAt 1 bytes
            addrType = fstByte .&. 0b11110000
            network  = fstByte .&. 0b00001111
            size = publicKeyHashSize
        in
            case addrType of
               -- 0000: base address: keyhash28,keyhash28
                0b00000000 | BS.length rest == 2 * size ->
                    Just $ unlines
                        [ "address style:      " <> "Shelley"
                        , "stake reference:    " <> "by value"
                        , "spending key hash:  " <> base16 (BS.take size rest)
                        , "stake key hash:     " <> base16 (BS.drop size rest)
                        , "network tag:        " <> show network
                        ]
               -- 0001: base address: scripthash28,keyhash28
                0b00010000 | BS.length rest == 2 * size ->
                    Just $ unlines
                        [ "address style:      " <> "Shelley"
                        , "stake reference:    " <> "by value"
                        , "script hash:        " <> base16 (BS.take size rest)
                        , "stake key hash:     " <> base16 (BS.drop size rest)
                        , "network tag:        " <> show network
                        ]
               -- 0010: base address: keyhash28,scripthash28
                0b00100000 | BS.length rest == 2 * size ->
                    Just $ unlines
                        [ "address style:      " <> "Shelley"
                        , "stake reference:    " <> "by value"
                        , "spending key hash:  " <> base16 (BS.take size rest)
                        , "stake script hash:  " <> base16 (BS.drop size rest)
                        , "network tag:        " <> show network
                        ]
               -- 0011: base address: scripthash28,scripthash28
                0b00110000 | BS.length rest == 2 * size ->
                    Just $ unlines
                        [ "address style:      " <> "Shelley"
                        , "stake reference:    " <> "by value"
                        , "script hash:        " <> base16 (BS.take size rest)
                        , "stake script hash:  " <> base16 (BS.drop size rest)
                        , "network tag:        " <> show network
                        ]
               -- 0100: pointer address: keyhash28, 3 variable length uint
               -- TODO Could fo something better for pointer and try decoding
               --      the pointer
                0b01000000 | BS.length rest > size -> do
                    ptr <- getPtr (BS.drop size rest)
                    Just $ unlines
                        [ "address style:      " <> "Shelley"
                        , "stake reference:    " <> "by pointer"
                        , "spending key hash:  " <> base16 (BS.take size rest)
                        , "pointer:            " <> prettyPtr ptr
                        , "network tag:        " <> show network
                        ]
               -- 0101: pointer address: scripthash28, 3 variable length uint
               -- TODO Could fo something better for pointer and try decoding
               --      the pointer
                0b01010000 | BS.length rest > size -> do
                    ptr <- getPtr (BS.drop size rest)
                    Just $ unlines
                        [ "address style:      " <> "Shelley"
                        , "stake reference:    " <> "by pointer"
                        , "script hash:        " <> base16 (BS.take size rest)
                        , "pointer:            " <> prettyPtr ptr
                        , "network tag:        " <> show network
                        ]
               -- 0110: enterprise address: keyhash28
                0b01100000 | BS.length rest == size ->
                    Just $ unlines
                        [ "address style:      " <> "Shelley"
                        , "stake reference:    " <> "none"
                        , "spending key hash:  " <> base16 (BS.take size rest)
                        , "network tag:        " <> show network
                        ]
               -- 0111: enterprise address: scripthash28
                0b01110000 | BS.length rest == size ->
                    Just $ unlines
                        [ "address style:      " <> "Shelley"
                        , "stake reference:    " <> "none"
                        , "script hash:        " <> base16 (BS.take size rest)
                        , "network tag:        " <> show network
                        ]
               -- 1000: byron address
                0b10000000 ->
                    inspectByronAddress addr <|> inspectIcarusAddress addr
                _ ->
                    Nothing
  where
    bytes  = unAddress addr
    base16 = T.unpack . T.decodeUtf8 . encode EBase16

    prettyPtr :: ChainPointer -> String
    prettyPtr ChainPointer{slotNum,transactionIndex,outputIndex} = unwords
        [ "sl#" <> show slotNum
        , "tx#" <> show transactionIndex
        , "ix#" <> show outputIndex
        ]

    getPtr :: ByteString -> Maybe ChainPointer
    getPtr source = case runGetOrFail get (BL.fromStrict source) of
        Left{} -> Nothing
        Right (rest, _, a) -> guard (BL.null rest) $> a
      where
        get = ChainPointer
            <$> getVariableLengthNat
            <*> getVariableLengthNat
            <*> getVariableLengthNat

-- Re-export from 'Cardano.Address' to have it documented specialized in Haddock.
--
-- | Convert a public key to a payment 'Address' valid for the given
-- network discrimination.
--
-- @since 2.0.0
paymentAddress
    :: NetworkDiscriminant Shelley
    -> Shelley 'AddressK XPub
    -> Address
paymentAddress =
    Internal.paymentAddress

-- | Extend an existing payment 'Address' to make it a delegation address.
--
-- @since 2.0.0
extendAddress
    :: Address
    -> Either (Shelley 'StakingK XPub) ChainPointer
    -> Either ErrExtendAddress Address
extendAddress addr stakeReference = do
    when (isNothing (inspectShelleyAddress addr)) $
        Left $ ErrInvalidAddressStyle "Given address isn't a Shelley address"

    let bytes = unAddress addr
    let (fstByte, rest) = first BS.head $ BS.splitAt 1 bytes

    when ((fstByte .&. 0b11110000) /= 0b01100000) $ do
        Left $ ErrInvalidAddressType "Only payment addresses can be extended"

    case stakeReference of
        Left stakingKey -> do
            pure $ unsafeMkAddress $ BL.toStrict $ runPut $ do
                putWord8 $ fstByte .&. 0b00001111
                putByteString rest
                putByteString . blake2b224 $ stakingKey

        Right pointer -> do
            pure $ unsafeMkAddress $ BL.toStrict $ runPut $ do
                putWord8 $ fstByte .&. 0b01001111
                putByteString rest
                putPointer pointer
  where
    putPointer (ChainPointer a b c) = do
        putVariableLengthNat a
        putVariableLengthNat b
        putVariableLengthNat c

-- | Captures error occuring when trying to extend an invalid address.
--
-- @since 2.0.0
data ErrExtendAddress
    = ErrInvalidAddressStyle String
    | ErrInvalidAddressType String
    deriving (Show)

-- Re-export from 'Cardano.Address' to have it documented specialized in Haddock.
--
-- | Convert a public key and a staking key to a delegation 'Address' valid
-- for the given network discrimination. Funds sent to this address will be
-- delegated according to the delegation settings attached to the delegation
-- key.
--
-- @since 2.0.0
delegationAddress
    :: NetworkDiscriminant Shelley
    -> Shelley 'AddressK XPub
    -> Shelley 'StakingK XPub
    -> Address
delegationAddress =
    Internal.delegationAddress


-- Re-export from 'Cardano.Address' to have it documented specialized in Haddock.
--
-- | Convert a public key and pointer to staking certificate in blockchain to a
-- pointer 'Address' valid for the given network discrimination.
--
-- @since 2.0.0
pointerAddress
    :: NetworkDiscriminant Shelley
    -> Shelley 'AddressK XPub
    -> ChainPointer
    -> Address
pointerAddress =
    Internal.pointerAddress

--
-- Network Discriminant
--

instance HasNetworkDiscriminant Shelley where
    type NetworkDiscriminant Shelley = NetworkTag
    addressDiscrimination _ = RequiresNetworkTag
    networkTag = id

-- | Error reported from trying to create a network discriminant from number
--
-- @since 2.0.0
newtype MkNetworkDiscriminantError
    = ErrWrongNetworkTag Integer
      -- ^ Wrong network tag.
    deriving (Eq, Show)

-- | Construct 'NetworkDiscriminant' for Cardano 'Shelley' from a number.
-- If the number is invalid, ie., not between 0 and 15, then
-- 'MkNetworkDiscriminantError' is thrown.
--
-- @since 2.0.0
mkNetworkDiscriminant
    :: Integer
    -> Either MkNetworkDiscriminantError (NetworkDiscriminant Shelley)
mkNetworkDiscriminant nTag
    | nTag < 16 =  Right $ NetworkTag $ fromIntegral nTag
    | otherwise = Left $ ErrWrongNetworkTag nTag

-- | 'NetworkDicriminant' for Cardano MainNet & Shelley
--
-- @since 2.0.0
shelleyMainnet :: NetworkDiscriminant Shelley
shelleyMainnet = NetworkTag 1

-- | 'NetworkDicriminant' for Cardano Testnet & Shelley
--
-- @since 2.0.0
shelleyTestnet :: NetworkDiscriminant Shelley
shelleyTestnet = NetworkTag 0

--
-- Unsafe
--

-- | Unsafe backdoor for constructing an 'Shelley' key from a raw 'XPrv'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 2.0.0
liftXPrv :: XPrv -> Shelley depth XPrv
liftXPrv = Shelley

-- | Unsafe backdoor for constructing an 'Shelley' key from a raw 'XPub'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 2.0.0
liftXPub :: XPub -> Shelley depth XPub
liftXPub = Shelley

--
-- Internal
--

-- Hash a public key
blake2b224 :: Shelley depth XPub -> ByteString
blake2b224 =
    BA.convert . hash @_ @Blake2b_224 . xpubPublicKey . getKey

-- Size, in bytes, of a hash of public key (without the corresponding chain code)
publicKeyHashSize :: Int
publicKeyHashSize = hashDigestSize Blake2b_224

-- Purpose is a constant set to 1852' (or 0x8000073c) following the BIP-44
-- extension for Cardano:
--
-- https://github.com/input-output-hk/implementation-decisions/blob/e2d1bed5e617f0907bc5e12cf1c3f3302a4a7c42/text/1852-hd-chimeric.md
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000073c

-- One master node (seed) can be used for unlimited number of independent
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

-- The minimum seed length for 'genMasterKeyFromMnemonic'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16
