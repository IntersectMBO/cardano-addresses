{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Command.ScriptSpec
    ( spec
    ) where

import Prelude

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

        specScriptInvalid ListTooSmall
            [iii|at_least 4 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptInvalid ListTooSmall
            [iii|at_least 1 [ #{verKeyH1}, at_least 2 [ #{verKeyH2} ] ]|]

        specScriptInvalid EmptyList
            [iii|all []|]

        specScriptInvalid EmptyList
            [iii|any [ #{verKeyH1}, all [] ]|]

        specScriptInvalid MZero
            [iii|at_least 0 [ #{verKeyH1}, #{verKeyH2} ]|]

        specScriptInvalid DuplicateSignatures
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH1}]|]

        specScriptInvalid Malformed
            [iii|script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxjq8egs9|]

specScriptHashProper :: String -> String -> SpecWith ()
specScriptHashProper expected script = it (script <> " => " <> expected) $ do
    out <- cli ["script", "hash", script] ""
    out `shouldBe` expected

specScriptInvalid :: ErrValidateScript -> String -> SpecWith ()
specScriptInvalid errMsg script = it (script <> " => " <> show errMsg) $ do
    (out, err) <- cli ["script", "hash", script] ""
    out `shouldBe` ("" :: String)
    err `shouldContain` (prettyErrValidateScript errMsg)

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
