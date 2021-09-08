{-# LANGUAGE CPP #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Main (main) where

import Prelude

import Cardano.Address.JSAPI
    ( startApi )
import Language.Javascript.JSaddle
    ( Object )
import Language.Javascript.JSaddle.Warp
    ( run )

#ifdef ghcjs_HOST_OS
foreign import javascript interruptible "cardanoAddressesInitComplete($1, $c);" initComplete
  :: Object -> IO ()
#else
initComplete = undefined
#endif

main :: IO ()
main = run 3757 (startApi >>= initComplete)
