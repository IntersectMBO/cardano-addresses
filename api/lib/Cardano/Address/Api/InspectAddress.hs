{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Cardano.Address.Api.InspectAddress (
  export
) where

import Prelude

import Cardano.Address (unsafeMkAddress)
import qualified Cardano.Address.Style.Shelley as Shelley
       (eitherInspectAddress)
import qualified Cardano.Codec.Bech32.Prefixes as CIP5 (root_xvk)
import Codec.Binary.Encoding
       (fromBase58, fromBech32, fromBase16, detectEncoding,
        AbstractEncoding(..))
import Control.Exception (SomeException)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Catch (Exception(..))
import Control.Monad.Error.Class
    ( Error )
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import GHCJS.Marshal (toJSVal_aeson)
import Language.Javascript.JSaddle
       (call, fun, jss, JSM, Object, fromJSValUnchecked)
import Options.Applicative.Derivation (xpubReader)

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

-- used for 'inspect'
instance Error SomeException

export :: Object -> JSM ()
export api =
    void $ api ^. jss "inspectAddress" (fun $ \ _ _ [rootPublicKeyJSVal, addressJSVal, onSuccess, onFail] -> do
      mbRootPublicKeyText :: Maybe String <- fromJSValUnchecked rootPublicKeyJSVal
      addressText :: Text <- fromJSValUnchecked addressJSVal
      case mapM (xpubReader [CIP5.root_xvk]) mbRootPublicKeyText of
        Left e ->
          void $ call onFail onFail [e]
        Right rootPublicKey ->
          case getBytes addressText of
            Left e ->
              void $ call onFail onFail [e]
            Right bytes ->
              case Shelley.eitherInspectAddress rootPublicKey (unsafeMkAddress bytes) of
                Right json -> do
                  jsVal <- toJSVal_aeson json
                  void $ call onSuccess onSuccess [jsVal]
                Left  e ->
                  void $ call onFail onFail [displayException e]
      )
