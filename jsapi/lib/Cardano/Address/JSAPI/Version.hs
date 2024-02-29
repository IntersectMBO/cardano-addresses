{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
import Control.Monad
    ( void )
import Data.Version
    ( showVersion )
import Language.Javascript.JSaddle
    ( JSM, Object, call, fun, jss )
import Paths_cardano_addresses_jsapi
    ( version )
import System.Git.TH
    ( gitRevParseHEAD )

export :: Object -> JSM ()
export api = api ^. jss "version" (fun $ \ _ _ -> impl)
  where
    impl (success:_) = void $ call success success [versionStr]
    impl _ = error "version: incorrect number of arguments"

    versionStr = showVersion version <> " @ " <> $(gitRevParseHEAD)
