{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Command.Script.PreimageSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( ErrValidateScript (..), prettyErrValidateScript )
import Data.String.Interpolate
    ( iii )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = do
    describeCmd [ "script", "preimage" ] $ do
        specScriptPreimageProper
            "008201818200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe"
            [iii|all [ #{verKeyH1} ]|]

        specScriptPreimageProper
            "008201818200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe"
            ("all    [ " <>verKeyH1<>"  ] ")

        specScriptPreimageProper
            "008201838200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f\
            \e8200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f333820058\
            \1c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f344"
            [iii|all [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptPreimageProper
            "008202818200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe"
            [iii|any [ #{verKeyH1} ]|]

        specScriptPreimageProper
            "008202838200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe\
            \8200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3338200581c\
            \3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f344"
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptPreimageProper
            "00830301838200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f\
            \3fe8200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3338200\
            \581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f344"
            [iii|at_least 1 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptPreimageProper
            "00830301828200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f\
            \3fe8201828200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3\
            \338200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f344"
            [iii|at_least 1 [ #{verKeyH1}, all [ #{verKeyH2}, #{verKeyH3} ] ]|]

        specScriptPreimageProper
            "008200581caf1c4aa560f374e106d458c78a0fe08428a5803105860b1697ca3f0a"
            [iii|#{verKeyH4}|]

        specScriptPreimageProper
            "008200581caf1c4aa560f374e106d458c78a0fe08428a5803105860b1697ca3f0a"
            [iii|#{verKey4}|]

        specScriptPreimageProper
            "008200581caf1c4aa560f374e106d458c78a0fe08428a5803105860b1697ca3f0a"
            [iii|#{verExtKey4}|]

        specScriptInvalid Malformed
            [iii|wrong [ #{verKeyH1} ]|]

        specScriptInvalid Malformed
            [iii|any [ #{verKeyH1}, ]|]

        specScriptPreimageProper
            "00830304838200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356e\
            \f320f3fe8200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef3\
            \20f3338200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f344"
            [iii|at_least 4 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptPreimageProper
            "00830301828200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356e\
            \f320f3fe830302818200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f333"
            [iii|at_least 1 [ #{verKeyH1}, at_least 2 [ #{verKeyH2} ] ]|]

        specScriptPreimageProper
            "00820180"
            [iii|all []|]

        specScriptPreimageProper
            "008202828200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe820180"
            [iii|any [ #{verKeyH1}, all [] ]|]

        specScriptPreimageProper
            "00830300828200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe8200\
            \581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f333"
            [iii|at_least 0 [ #{verKeyH1}, #{verKeyH2} ]|]

        specScriptPreimageProper
            "00830301848200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe8200\
            \581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f33382040a82051819"
            [iii|at_least 1 [ #{verKeyH1}, #{verKeyH2}, active_from 10, active_until 25 ]|]

        specScriptPreimageProper
            "008202838200581c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe820058\
            \1c3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3338200581c3c07030e36bf\
            \ffe67e2e2ec09e5293d384637cd2f004356ef320f3fe"
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH1}]|]

        specScriptInvalid Malformed
            [iii|script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxjq8egs9|]

        specScriptInvalid Malformed
            [iii|any [ #{verKeyH1}, #{verKeyH2}, active_from a]|]

specScriptPreimageProper :: String -> String -> SpecWith ()
specScriptPreimageProper expected script = it (script <> " => " <> expected) $ do
    out <- cli ["script", "preimage", script] ""
    out `shouldBe` expected

specScriptInvalid :: ErrValidateScript -> String -> SpecWith ()
specScriptInvalid errMsg script = it (script <> " => " <> show errMsg) $ do
    (out, err) <- cli ["script", "preimage", script] ""
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
