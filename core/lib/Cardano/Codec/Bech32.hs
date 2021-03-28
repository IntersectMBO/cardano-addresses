{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Codec.Bech32 (bech32With,fromBech32With) where

import Prelude

import Codec.Binary.Bech32
    ( HumanReadablePart, humanReadablePartToText )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import qualified Codec.Binary.Encoding as E
import Data.ByteString
    ( ByteString )
import Data.Foldable
    ( fold )
import Data.Text
    ( Text )
import qualified Data.Text.Encoding as T

-- | Encode an address to bech32 'Text', with a prefix depending on type.
--
-- @since 3.4.0
bech32With :: HumanReadablePart -> ByteString -> Text
bech32With hrp =
    T.decodeUtf8 . encode (EBech32 hrp)

fromBech32With :: HumanReadablePart -> (ByteString -> b) -> Text -> Either String b
fromBech32With hrn con text = do
  (hrp, bs) <- E.fromBech32 (const id) $ T.encodeUtf8 text
  if hrp == hrn then
    pure $ con bs
  else
    Left $ fold
        [ "Human Readable Part should be "
        , show (humanReadablePartToText hrn)
        , " but we parsed "
        , show (humanReadablePartToText hrp)
        ]
