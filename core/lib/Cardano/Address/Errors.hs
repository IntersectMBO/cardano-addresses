{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0

module Cardano.Address.Errors
    ( -- $overview

      -- * AddrError
      ShelleyAddrError(..)
    , ByronAddrError(..)
    , IcarusAddrError(..)

      -- * pretty printers
    , prettyShError
    , prettyBrError
    , prettyIcError
  ) where

import Prelude

import Control.Exception
    ( Exception, displayException )
import Fmt
    ( format )


-- $overview
--
-- This module provides error/exception types for Addresses.

data ShelleyAddrError
    = ShUnknownAddrType
    | ShWrongInputSize Int -- the actual size
    | ShPtrRetrieveError String -- human readable error of underlying operation

deriving instance Show ShelleyAddrError

instance Exception ShelleyAddrError where
  displayException = prettyShError

prettyShError :: ShelleyAddrError -> String
prettyShError ShUnknownAddrType = "Unknown address type"
prettyShError (ShWrongInputSize i) = format "Wrong input size of {}" i
prettyShError (ShPtrRetrieveError s) = format "Failed to retrieve pointer (underlying errors was: {})" s


data ByronAddrError
    = BrMissingExpectedDerivationPath
    | forall e . (Exception e, Show e) => BrDeserialiseError e
    | BrFailedToDecryptPath

deriving instance Show ByronAddrError

instance Exception ByronAddrError where
  displayException = prettyBrError

prettyBrError :: ByronAddrError -> String
prettyBrError BrMissingExpectedDerivationPath = "Missing expected derivation path"
prettyBrError (BrDeserialiseError e) = format "Deserialisation error (was: {})" (show e)
prettyBrError BrFailedToDecryptPath = "Failed to decrypt derivation path"


data IcarusAddrError
    = IcUnexpectedDerivationPath
    | forall e . (Exception e, Show e) => IcDeserialiseError e

deriving instance Show IcarusAddrError

instance Exception IcarusAddrError where
  displayException = prettyIcError

prettyIcError :: IcarusAddrError -> String
prettyIcError IcUnexpectedDerivationPath = "Unexpected derivation path"
prettyIcError (IcDeserialiseError e) = format "Deserialisation error (was: {})" (show e)
