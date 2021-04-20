-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- GHCJS compatibility support.

module Cardano.Address.Jsbits
    ( addJsbitsDependency
    ) where

import Prelude

-- | This function does nothing except convince GHC that there is a dependency
-- on the cardano-addresses-jsbits package.
--
-- This workaround will make sure that GHCJS includes the @.js@ files from this
-- package when linking.
--
-- For non-GHCJS it really does nothing.
addJsbitsDependency :: IO ()
addJsbitsDependency = return ()
