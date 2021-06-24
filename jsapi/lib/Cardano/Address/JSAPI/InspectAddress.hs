{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0

module Cardano.Address.JSAPI.InspectAddress
    ( export
    ) where

import Prelude

import Cardano.Address
    ( unsafeMkAddress )
import Codec.Binary.Encoding
    ( AbstractEncoding (..)
    , detectEncoding
    , fromBase16
    , fromBase58
    , fromBech32
    )
import Control.Lens
    ( (^.) )
import Control.Monad
    ( void )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import GHCJS.Marshal
    ( toJSVal_aeson )
import Language.Javascript.JSaddle
    ( JSM
    , JSVal
    , Object
    , call
    , create
    , fromJSValUnchecked
    , fun
    , jss
    , toJSVal
    , val
    )
import Options.Applicative.Derivation
    ( xpubReader )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

getBytes :: Text -> Either String ByteString
getBytes text =
    case detectEncoding (T.unpack text) of
        Just EBase16    -> fromBase16 raw
        Just EBech32 {} -> (fmap snd . fromBech32 (\ _ x -> x)) raw
        Just EBase58    -> fromBase58 raw
        Nothing         -> Left
            "Couldn't detect input encoding? Data on stdin must be encoded as \
            \bech16, bech32 or base58."
  where
    raw = T.encodeUtf8 text

errorString :: String -> String -> JSM JSVal
errorString code msg = do
    err <- create
    ob <- create
    ob ^. jss "code" (val code)
    ob ^. jss "error" err
    ob ^. jss "message" (val msg)
    toJSVal ob

export :: Object -> JSM ()
export api = void $ api ^. jss "inspectAddress" (fun $ \ _ _ -> impl)
  where
    impl [rootVKeyJSVal, addressJSVal, onSuccess, onFail] = void $ do
        jsInspect rootVKeyJSVal addressJSVal >>= \case
            Left e -> call onFail onFail [e]
            Right a -> call onSuccess onSuccess [a]
    impl _ = error "inspectAddress: incorrect number of arguments"

    jsInspect rootVKeyJSVal addressJSVal = do
        mbRootPublicKeyText :: Maybe String <- fromJSValUnchecked rootVKeyJSVal
        addressText :: Text <- fromJSValUnchecked addressJSVal
        case mapM (xpubReader [CIP5.root_xvk]) mbRootPublicKeyText of
          Left e -> Left <$> errorString "xvk" e
          Right rootPublicKey -> case getBytes addressText of
              Left e -> Left <$> errorString "decode" e
              Right bs -> either
                 (fmap Left . toJSVal_aeson)
                 (fmap Right . toJSVal_aeson)
                 (Shelley.eitherInspectAddress rootPublicKey (unsafeMkAddress bs))
