{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0

module Cardano.Address.JSAPI.Version
    ( export
    ) where

import Prelude

import Control.Lens
    ( (^.) )
import Data.Version
    ( showVersion )
import Language.Javascript.JSaddle
    ( JSM, Object, call, fun, jss )
import Paths_cardano_addresses_jsapi
    ( version )
import System.Git.TH
    ( gitRevParseHEAD )

export :: Object -> JSM ()
export api =
  api ^. jss "version" (fun $ \ _ _ [success] -> do
    _ <- call success success [showVersion version <> " @ " <> $(gitRevParseHEAD)]
    pure ())
