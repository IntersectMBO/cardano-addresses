{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Address.Api.Version (
  export
) where

import Prelude

import System.Git.TH
    ( gitRevParseHEAD )

import Paths_cardano_addresses_api
    ( version )
import Language.Javascript.JSaddle (call, fun, jss, Object, JSM)
import Control.Lens ((^.))
import Data.Version (showVersion)

export :: Object -> JSM ()
export api =
  api ^. jss "version" (fun $ \ _ _ [success] -> do
    _ <- call success success [showVersion version <> " @ " <> $(gitRevParseHEAD)]
    pure ())
