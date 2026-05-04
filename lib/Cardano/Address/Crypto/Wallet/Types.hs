{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Vendored from @cardano-crypto@ (Apache-2.0).
-- Original: Cardano.Crypto.Wallet.Types
module Cardano.Address.Crypto.Wallet.Types
    ( ChainCode (..)
    , DerivationScheme (..)
    , DerivationIndex
    , pattern LatestScheme
    ) where

import Prelude

import Control.DeepSeq
    ( NFData )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteString
    ( ByteString )
import Data.Hashable
    ( Hashable )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

type DerivationIndex = Word32

data DerivationScheme = DerivationScheme1 | DerivationScheme2
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)
    deriving anyclass NFData

pattern LatestScheme :: DerivationScheme
pattern LatestScheme = DerivationScheme2

newtype ChainCode = ChainCode ByteString
    deriving newtype (Show, Eq, Ord, ByteArrayAccess, NFData, Hashable)
