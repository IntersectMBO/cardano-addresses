{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Encoding
    (
    -- * Types
      AbstractEncoding(..)
    , Encoding

    -- * Applicative Parser
    , encodingOpt
    ) where

import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), Encoding )
import Control.Applicative
    ( (<|>) )
import Options.Applicative
    ( Parser, flag, flag', long )

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
