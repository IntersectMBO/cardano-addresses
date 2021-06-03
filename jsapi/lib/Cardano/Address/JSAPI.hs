-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0

module Cardano.Address.JSAPI
    ( startApi
    ) where

import Prelude

import Cardano.Address.Compat
    ( ghcjsBuildSupport )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Language.Javascript.JSaddle
    ( JSM, Object, obj )

import qualified Cardano.Address.JSAPI.InspectAddress as InspectAddress
import qualified Cardano.Address.JSAPI.Version as Version

startApi :: JSM Object
startApi = do
  liftIO ghcjsBuildSupport
  api <- obj
  Version.export api
  InspectAddress.export api
  pure api
