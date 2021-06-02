module Cardano.Address.Api
    ( startApi
    ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import qualified Cardano.Address.Api.InspectAddress as InspectAddress
import qualified Cardano.Address.Api.Version as Version
import Cardano.Address.Compat (ghcjsBuildSupport)
import Language.Javascript.JSaddle (obj, JSM, Object)

startApi :: JSM Object
startApi = do
  liftIO ghcjsBuildSupport
  api <- obj
  Version.export api
  InspectAddress.export api
  pure api
