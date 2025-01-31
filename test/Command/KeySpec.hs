{-# LANGUAGE FlexibleContexts #-}

module Command.KeySpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, xprvToBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Data.List
    ( isInfixOf )
import Options.Applicative.Derivation
    ( DerivationIndex, derivationIndexToString, firstHardened )
import Test.Hspec
    ( Spec )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Property, counterexample, label )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.Utils
    ( cli, describeCmd )

import Test.Arbitrary
    ()

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describeCmd ["key"] $ do
    prop "public/child commute" prop_publicKeyDerivation

-- | For soft indices, public key derivation should be "equivalent" to private
-- key derivation.
--
-- I.e. The following diagram should commute:
--
-- @
--                        key public
--
--              xprv +-----------------> xpub
--                +                       +
--                |                       |
--                |                       |
--      key child |                       | key child
--                |                       |
--                |                       |
--                v                       v
--             xprv' +-----------------> xpub'
--
--                        key public
-- @
prop_publicKeyDerivation
    :: DerivationIndex
    -> XPrv
    -> Property
prop_publicKeyDerivation ix xprv = do
    if ix < firstHardened then monadicIO $ do
        monitor (label "Soft Index")
        out1 <- run (public bytes >>= child)
        out2 <- run (child  bytes >>= public)
        monitor (counterexample ("stdout: " <> out1))
        monitor (counterexample ("stdout: " <> out2))
        assert (out1 == out2)
    else monadicIO $ do
        monitor (label "Hard Index")
        (out, err) <- run (public bytes >>= cli ["key", "child", "0/" <> derivationIndexToString ix ])
        monitor (counterexample ("stdout: " <> out))
        monitor (counterexample ("stderr: " <> err))
        assert (out == "")
        assert ("you must use soft indexes only" `isInfixOf` err)
  where
    bytes  = T.unpack $ T.decodeUtf8 $ encode (EBech32 CIP5.acct_xsk) $ xprvToBytes xprv
    public = cli [ "key", "public", "--with-chain-code" ]
    child  = cli [ "key", "child", "0/" <> derivationIndexToString ix ]
