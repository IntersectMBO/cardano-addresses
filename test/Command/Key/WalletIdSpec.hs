{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Command.Key.WalletIdSpec
    ( spec
    ) where

import Prelude

import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.Monad
    ( forM_ )
import Data.Char
    ( isHexDigit )
import Data.Either
    ( isLeft )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( Spec
    , SpecWith
    , expectationFailure
    , it
    , shouldBe
    , shouldContain
    , shouldNotBe
    , shouldSatisfy
    )
import Test.Utils
    ( cli, describeCmd )

import Command.Key.WalletId
    ( payloadToHash )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describeCmd [ "key", "walletid" ] $ do
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/0/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/0/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/1/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/1/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/2/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/2/0" "--with-chain-code"

    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/0/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/0/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/1/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/1/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/2/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/2/0" "--with-chain-code"

    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/0/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/0/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/1/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/1/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/2/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/2/0" "--with-chain-code"

    specAcctKeyNotExtended "shelley" "1852H/1815H/0H"
    specAcctKeyNotExtended "icarus" "1852H/1815H/0H"
    specAcctKeyNotExtended "shared" "1854H/1815H/0H"

    specRootKeyNotExtended "shelley"
    specRootKeyNotExtended "icarus"
    specRootKeyNotExtended "shared"

    specRootKeyPubPrvHasEqualWalletId "shelley"
    specRootKeyPubPrvHasEqualWalletId "icarus"

    specAcctKeyPubPrvHasEqualWalletId "shelley" "1852H/1815H/0H" Nothing Nothing
    specAcctKeyPubPrvHasEqualWalletId "icarus" "1852H/1815H/0H" Nothing Nothing
    specAcctKeyPubPrvHasEqualWalletId "shared" "1854H/1815H/0H" (Just "cosigner#0") Nothing
    specAcctKeyPubPrvHasEqualWalletId "shared" "1854H/1815H/1H" (Just "cosigner#0") Nothing
    specAcctKeyPubPrvHasEqualWalletId "shared" "1854H/1815H/10H" (Just "cosigner#0") Nothing
    specAcctKeyPubPrvHasEqualWalletId "shared" "1854H/1815H/0H" (Just "cosigner#0") (Just "cosigner#0")
    specAcctKeyPubPrvHasEqualWalletId "shared" "1854H/1815H/0H" (Just "cosigner#0") (Just "cosigner#0")
    specAcctKeyPubPrvHasEqualWalletId "shared" "1854H/1815H/0H" (Just "all [cosigner#0,cosigner#1]") (Just "cosigner#0")
    specAcctKeyPubPrvHasEqualWalletId "shared" "1854H/1815H/0H" (Just "cosigner#0") (Just "any [cosigner#1, active_until 1000]")

    specInvalidXPrvLength "root" CIP5.root_xsk
    specInvalidXPrvLength "root shared" CIP5.root_shared_xsk
    specInvalidXPrvLength "account" CIP5.acct_xsk
    specInvalidXPrvLength "shared account" CIP5.acct_shared_xsk

    specBogusWalletIdNotEmitted

    specRootSharedKeyWalletId

    specPayloadToHash


-- | Encode raw bytes as a bech32 string with the given prefix.
bech32String :: HumanReadablePart -> BS.ByteString -> String
bech32String hrp = T.unpack . T.decodeUtf8 . encode (EBech32 hrp)

specKeyNeitherRootNorAcct :: String -> String -> String -> SpecWith ()
specKeyNeitherRootNorAcct style path cc = it "fails if key is nether root nor account" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", style ]
              >>= cli [ "key", "child", path ]
              >>= cli [ "key", "public", cc ]
              >>= cli [ "key", "walletid"]
    out `shouldBe` ""
    err `shouldContain` "Invalid human-readable prefix."

specAcctKeyNotExtended :: String -> String -> SpecWith ()
specAcctKeyNotExtended style path = it "fails if account key is not extended" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", style ]
              >>= cli [ "key", "child", path ]
              >>= cli [ "key", "public", "--without-chain-code" ]
              >>= cli [ "key", "walletid"]
    out `shouldBe` ""
    err `shouldContain` "Invalid human-readable prefix."

specRootKeyNotExtended :: String -> SpecWith ()
specRootKeyNotExtended style = it "fails if root key is not extended" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", style ]
              >>= cli [ "key", "public", "--without-chain-code" ]
              >>= cli [ "key", "walletid"]
    out `shouldBe` ""
    err `shouldContain` "Invalid human-readable prefix."

specRootKeyPubPrvHasEqualWalletId :: String -> SpecWith ()
specRootKeyPubPrvHasEqualWalletId style = it "root private key and its public key give the same wallet id" $ do
    xprv <- cli [ "recovery-phrase", "generate" ] ""
        >>= cli [ "key", "from-recovery-phrase", style ]

    walletidFromXPrv <- cli @String [ "key", "walletid"] xprv
    walletidFromXPub <- cli [ "key", "public", "--with-chain-code" ] xprv
                    >>= cli [ "key", "walletid"]

    walletidFromXPrv `shouldBe` walletidFromXPub

specAcctKeyPubPrvHasEqualWalletId
    :: String
    -> String
    -> Maybe String
    -> Maybe String
    -> SpecWith ()
specAcctKeyPubPrvHasEqualWalletId style path spendingScriptM stakingScriptM =
    it "root private key and its public key give the same wallet id" $ do
    xprv <- cli [ "recovery-phrase", "generate" ] ""
        >>= cli [ "key", "from-recovery-phrase", style ]
        >>= cli [ "key", "child", path ]

    let spendingArg =
            case spendingScriptM of
                Nothing -> []
                Just spendingScript -> ["--spending", spendingScript]
    let stakingArg =
            case stakingScriptM of
                Nothing -> []
                Just stakingScript -> ["--staking", stakingScript]

    walletidFromXPrv <- cli @String ([ "key", "walletid"] ++ spendingArg ++ stakingArg) xprv
    walletidFromXPub <- cli [ "key", "public", "--with-chain-code" ] xprv
                    >>= cli ([ "key", "walletid"] ++ spendingArg ++ stakingArg)

    walletidFromXPrv `shouldBe` walletidFromXPub

specInvalidXPrvLength :: String -> HumanReadablePart -> SpecWith ()
specInvalidXPrvLength name hrp = it ("fails if " ++ name ++ " private key has wrong length") $ do
    let input = bech32String hrp (BS.replicate 95 0xFF)
    (exit, out, err) <- cli @(ExitCode, String, String) [ "key", "walletid" ] input
    exit `shouldNotBe` ExitSuccess
    out `shouldBe` ""
    err `shouldContain` "data should be a 96-byte private key."

-- | Regression test for the deterministic fake wallet id that was produced
-- when an invalid extended private key was silently hashed.  The exact id is
-- derived from the old error-string payload, so we make sure it can never be
-- emitted for a 96-byte xsk input.
specBogusWalletIdNotEmitted :: SpecWith ()
specBogusWalletIdNotEmitted = it "does not emit the deterministic fake wallet id for a 96-byte xsk" $ do
    let bogusWalletId = "84fa23bd2cd3e1265628884b52857d47f5b0b01f"
    forM_ [ CIP5.root_xsk
          , CIP5.root_shared_xsk
          , CIP5.acct_xsk
          , CIP5.acct_shared_xsk
          ] $ \hrp -> do
        let input = bech32String hrp (BS.replicate 96 0xFF)
        (exit, out, _err) <- cli @(ExitCode, String, String) [ "key", "walletid" ] input
        case exit of
            ExitSuccess ->
                out `shouldNotBe` bogusWalletId
            _ ->
                pure ()

specRootSharedKeyWalletId :: SpecWith ()
specRootSharedKeyWalletId = it "accepts a valid root_shared_xsk and produces a wallet id" $ do
    xprv <- cli [ "recovery-phrase", "generate" ] ""
        >>= cli [ "key", "from-recovery-phrase", "shared" ]

    walletid <- cli @String [ "key", "walletid" ] xprv
    case words walletid of
        (wid:_) -> do
            length wid `shouldBe` 40
            all isHexDigit wid `shouldBe` True
        _ ->
            expectationFailure "expected a non-empty wallet id"

specPayloadToHash :: SpecWith ()
specPayloadToHash = do
    it "payloadToHash fails for a short private key" $ do
        payloadToHash CIP5.root_xsk (BS.replicate 95 0xFF)
            `shouldSatisfy` isLeft

    it "payloadToHash returns public key bytes unchanged" $ do
        let bs = BS.replicate 64 0xAB
        payloadToHash CIP5.root_xvk bs `shouldBe` Right bs
