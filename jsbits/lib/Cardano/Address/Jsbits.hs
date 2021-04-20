module Cardano.Address.Jsbits (addJsbitsDependency) where

import Prelude

-- Work around to make sure GHCJS includes the this package when linking
addJsbitsDependency :: IO ()
addJsbitsDependency = return ()

