{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Command.ScriptSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( TxValidity (..), ValidationLevel (..) )
import Cardano.Address.Script.Parser
    ( ErrValidateScript (..), prettyErrValidateScript )
import Data.String.Interpolate
    ( iii )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = do
    describeCmd [ "script", "hash" ] $ do
        specScriptHashProper "script102cyqtxktt547mcu3ea6v0ghaj6tjpw5ua34llyrxq02cg3yew2"
            [iii|all [ #{verKeyH1} ]|]

        specScriptHashProper "script102cyqtxktt547mcu3ea6v0ghaj6tjpw5ua34llyrxq02cg3yew2"
            ("all    [ " <>verKeyH1<>"  ] ")

        specScriptHashProper "script1tcn27mnwqdptdufkqxf0tsn6q7npttm56y2xk3k3tkzeklgpray"
            [iii|all [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptHashProper "script1w7r7nyfe3wtwz9caslxe35q534zl308cpft58qdgv3wq25j9e5x"
            [iii|any [ #{verKeyH1} ]|]

        specScriptHashProper "script16fdf0plkpdwf3rl9u74dp4qjdq3k8mg0qrqyttl9ne26yx4w2tr"
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptHashProper "script16j8c8vy8t9nektczn5m47evxexkvewwcdnug45htjds7qdcdjgt"
            [iii|at_least 1 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptHashProper "script1dalltthzqnpd06fukp6j4s026wsnwrrf9k0lytzg0hnj64tfxdm"
            [iii|at_least 1 [ #{verKeyH1}, all [ #{verKeyH2}, #{verKeyH3} ] ]|]

        specScriptHashProper "script1gtq9tzn6c82dzjquvn6aw5kj3s2rny6au3lejsymf96azkvdfqc"
            [iii|#{verKeyH4}|]

        specScriptHashProper "script1gtq9tzn6c82dzjquvn6aw5kj3s2rny6au3lejsymf96azkvdfqc"
            [iii|#{verKey4}|]

        specScriptHashProper "script1gtq9tzn6c82dzjquvn6aw5kj3s2rny6au3lejsymf96azkvdfqc"
            [iii|#{verExtKey4}|]

        specScriptInvalid Malformed
            [iii|wrong [ #{verKeyH1} ]|]

        specScriptInvalid Malformed
            [iii|any [ #{verKeyH1}, ]|]

        specScriptHashProper "script10w5mpzsfwcjeg4ksqh7ffl0ya4fda0yyra7z7c3je6g67x57ctn"
            [iii|at_least 4 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptHashProper "script16app2n3qlwc543cmvrlnxt29nrpqttu2kc72ftj5fapvzccsc6y"
            [iii|at_least 1 [ #{verKeyH1}, at_least 2 [ #{verKeyH2} ] ]|]

        specScriptHashProper "script163qjya2n5rc6je07ultq5rmjfvmgm5dam0pqsuc0en4u7967saj"
            [iii|all []|]

        specScriptHashProper "script1duhpq0x5k2tey5rltut0ucunhqagxfhwqyu5rh4jmrw2qrjtkus"
            [iii|any [ #{verKeyH1}, all [] ]|]

        specScriptHashProper "script1njfxl9tzwfqaxk8tt5llh9zdfse9srvpeunjssjslrhvvl0ys30"
            [iii|at_least 0 [ #{verKeyH1}, #{verKeyH2} ]|]

        specScriptHashProper "script12zfnrxhgn9xmzpptgz2eaepf8w9qww460xm0ek453y8qx3kfk7p"
            [iii|at_least 1 [ #{verKeyH1}, #{verKeyH2}, active_from 10, active_until 25 ]|]

        specScriptHashProper "script1uan07g0z30cfpvg7gxp5fmvjmv6ylaqa84msw8twyt5777hf7gf"
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH1}]|]

        specScriptInvalid Malformed
            [iii|script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxjq8egs9|]

        specScriptInvalid Malformed
            [iii|any [ #{verKeyH1}, #{verKeyH2}, active_from a]|]

    describeCmd [ "script", "validate"] $ do
        specScriptValidated RequiredValidation (TxValidity Nothing Nothing)
            [iii|#{verKey4}|]

        specScriptValidated RequiredValidation (TxValidity Nothing Nothing)
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, active_from 10, active_until 25]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, active_from 6, active_until 15]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, active_from 6, active_until 25]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|all []|]

        specScriptNotValidated Malformed RequiredValidation (TxValidity Nothing Nothing)
            [iii|any [ #{verKeyH1}, #{verKeyH2}, active_from a]|]

        specScriptNotValidated EmptyList RecommendedValidation (TxValidity Nothing Nothing)
            [iii|all []|]

        specScriptNotValidated LedgerIncompatible RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ active_from 11, active_until 25]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1},  active_from 11, active_until 25]|]

        specScriptNotValidated ListTooSmall RecommendedValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1},  active_from 11, active_until 25]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1},  active_from 11, active_until 25, active_until 30]|]

        specScriptNotValidated RedundantTimelocks RecommendedValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 1 [ #{verKeyH1},  active_from 11, active_until 25, active_until 30]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH2}]|]

        specScriptNotValidated DuplicateSignatures RecommendedValidation (TxValidity (Just 10) (Just 15))
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH2}]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 0 [ #{verKeyH1}, #{verKeyH2} ]|]

        specScriptNotValidated MZero RecommendedValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 0 [ #{verKeyH1}, #{verKeyH2} ]|]

specScriptHashProper :: String -> String -> SpecWith ()
specScriptHashProper expected script = it (script <> " => " <> expected) $ do
    out <- cli ["script", "hash", script] ""
    out `shouldBe` expected

specScriptInvalid :: ErrValidateScript -> String -> SpecWith ()
specScriptInvalid errMsg script = it (script <> " => " <> show errMsg) $ do
    (out, err) <- cli ["script", "hash", script] ""
    out `shouldBe` ("" :: String)
    err `shouldContain` (prettyErrValidateScript errMsg)

levelStr :: ValidationLevel -> String
levelStr = \case
    RequiredValidation -> "--required"
    RecommendedValidation -> "--recommended"

validityStr :: TxValidity -> [String]
validityStr = \case
    TxValidity Nothing Nothing -> []
    TxValidity (Just i) Nothing -> ["--tx-valid-from",show i]
    TxValidity Nothing (Just i) -> ["--tx-valid-to",show i]
    TxValidity (Just i) (Just j) -> ["--tx-valid-from",show i,"--tx-valid-to",show j]

specScriptValidated :: ValidationLevel -> TxValidity -> String -> SpecWith ()
specScriptValidated level validity script = it (script <> " => Validated.") $ do
    out <- cli (["script", "validate", levelStr level] ++ validityStr validity ++ [script]) ""
    out `shouldBe` "Validated.\n"

specScriptNotValidated :: ErrValidateScript -> ValidationLevel -> TxValidity -> String -> SpecWith ()
specScriptNotValidated errMsg level validity script = it (script <> " => " <> show errMsg) $ do
    (out, err) <- cli (["script", "validate", levelStr level] ++ validityStr validity ++ [script]) ""
    out `shouldBe` ("" :: String)
    err `shouldContain` prettyErrValidateScript errMsg

verKeyH1 :: String
verKeyH1 = "script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyreluzt36ms"

verKeyH2 :: String
verKeyH2 = "script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyrenxv223vj"

verKeyH3 :: String
verKeyH3 = "script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyre5g2sfvk2"

verExtKey4 :: String
verExtKey4 = "script_xvk1mjr5lrrlxuvelx94hu2cttmg5pp6cwy5h0sa37qvpcd07pv9g23nlvugj5ez9qfxxvkmjwnpn69s48cv572phfy6qpmnwat0hwcdrasapqewe"

verKey4 :: String
verKey4 = "script_vk1mjr5lrrlxuvelx94hu2cttmg5pp6cwy5h0sa37qvpcd07pv9g23skqaly0"

verKeyH4 :: String
verKeyH4 = "script_vkh14uwy4ftq7d6wzpk5trrc5rlqss52tqp3qkrqk95hegls54e0sga"
