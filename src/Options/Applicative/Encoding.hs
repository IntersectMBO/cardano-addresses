{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Encoding
    (
    -- * Types
      AbstractEncoding(..)
    , Encoding

    -- * Applicative Parser
    , encodingOpt
    ) where

import Prelude

import Codec.Binary.Bech32
    ( HumanReadablePart )
import Control.Applicative
    ( (<|>) )
import Options.Applicative
    ( Parser, flag, flag', long )


--
-- Types
--

-- | A concrete 'Encoding' algebraic data-type.
type Encoding = AbstractEncoding HumanReadablePart

-- | An abstract 'Encoding' to make it easy to map over the bech32 component.
-- Typically used as 'AbstractEncoding HumanReadablePart'.
--
-- > λ> let xpubHRP = [humanReadablePart|xpub|]
-- > λ> let xprvHRP = [humanReadablePart|xprv|]
-- >
-- > λ> fmap (const xpubHRP) (EBech32 xprvHRP)
-- > EBech32 (HumanReadablePart "xpub")
--
data AbstractEncoding a
    = EBase16
    | EBase58
    | EBech32 a
    deriving (Eq, Show, Functor)


--
-- Applicative Parser
--

-- | Parse an 'Encoding' from the command-line, as set of non-overlapping flags.
encodingOpt :: HumanReadablePart -> Parser Encoding
encodingOpt hrp = base16Flag <|> base58Flag <|> bech32Flag
  where
    base16Flag = flag'  EBase16 (long "base16")
    base58Flag = flag'  EBase58 (long "base58")
    bech32Flag = flag (EBech32 hrp) (EBech32 hrp) (long "bech32")
