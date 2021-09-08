{-# LANGUAGE CPP #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Main (main) where

import Prelude

import Cardano.Address.JSAPI
    ( startApi )
#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle
    ( Object )
#else
import Control.Lens
    ( (^.) )
import Control.Monad
    ( void )
import Control.Monad.IO.Class
    ( liftIO )
import Language.Javascript.JSaddle
    ( JSM, Object, fromJSValUnchecked, fun, js1 )
#endif
import Language.Javascript.JSaddle.Warp
    ( run )

#ifdef ghcjs_HOST_OS
foreign import javascript interruptible "testStart($1, $c);" initComplete
  :: Object -> IO ()
#else
initComplete :: Object -> JSM ()
initComplete api =
  void $ api ^. js1 "version" (fun $ \ _ _ [ver] ->
    fromJSValUnchecked ver >>= (liftIO . putStrLn))
#endif

-- When runing the native version, point a browser at http://localhost:3757/
main :: IO ()
main = run 3757 (startApi >>= initComplete)
