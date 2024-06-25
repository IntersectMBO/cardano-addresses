{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Command.Script.HashSpec
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
    describeCmd [ "script", "hash" ] $ do
        specScriptHashProper "script1ugvvxx2vrajnx7q4y8a3mtjgrms8a3c85zz5hjvwa2gpqpkhpzq"
            [iii|all [ #{verKeyH1} ]|]

        specScriptHashProper "script1ugvvxx2vrajnx7q4y8a3mtjgrms8a3c85zz5hjvwa2gpqpkhpzq"
            ("all    [ " <>verKeyH1<>"  ] ")

        specScriptHashProper "script1s7uwytqrwv63wp8e8cu7jz7j0nmqfcw3lmeh8u8dujmkvafpkpf"
            [iii|all [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptHashProper "script1tgp32rg8358hae5senu7degf4cdp3cvul5ylq5sy76nkxvlm48l"
            [iii|any [ #{verKeyH1} ]|]

        specScriptHashProper "script1wnsgkncprsznm9smhc6x9h2gms3dn7kyx5g549hjueussc5twaa"
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptHashProper "script1vw9etsd8d52dndc4aqkgpp23pmj2j9u29dayed494dyngpc9rsv"
            [iii|at_least 1 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptHashProper "script1hwv9dxq42uqy34las3gvhxg3s45n8vtzn74wjq6ungpp7xg86mh"
            [iii|at_least 1 [ #{verKeyH1}, all [ #{verKeyH2}, #{verKeyH3} ] ]|]

        specScriptHashProper "script1w8469gq5ed7xtyf2tqdng5yn7ykgckkfcl38xre8hk3ejk2lcwt"
            [iii|#{verKeyH4}|]

        specScriptInvalid Malformed
            [iii|wrong [ #{verKeyH1} ]|]

        specScriptInvalid Malformed
            [iii|any [ #{verKeyH1}, ]|]

        specScriptHashProper "script1pcjctr4ltcndy3nljsdrlv3jcawanz4kcj69aj00py62udt0j3g"
            [iii|at_least 4 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptHashProper "script10tx7wh633277e0dgw3mkwvmdawrjvdmcz88ypqjzsgxcjr9nwlj"
            [iii|at_least 1 [ #{verKeyH1}, at_least 2 [ #{verKeyH2} ] ]|]

        specScriptInvalid NotUniformKeyType
            [iii|all []|]

        specScriptHashProper "script14uj40hnew0uxrwlfz45z5umqwrs54kd0c04ujzyatyxzsk59wr8"
            [iii|any [ #{verKeyH1}, all [] ]|]

        specScriptHashProper "script1v9rsc0jdf8l7hm5y45hecm5phjl6lscxmanxm0s93rg3z8q25jj"
            [iii|at_least 0 [ #{verKeyH1}, #{verKeyH2} ]|]

        specScriptHashProper "script1ltujlnyeee7j5ujgjjw6taqc9vqlaj63ws75ttpfzxq9557zuzv"
            [iii|at_least 1 [ #{verKeyH1}, #{verKeyH2}, active_from 10, active_until 25 ]|]

        specScriptHashProper "script1mt0mww34xff9s6vzt6ehw633njcrd7am406e8vm6c66uggynax4"
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH1}]|]

        specScriptInvalid Malformed
            [iii|script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxjq8egs9|]

        specScriptInvalid Malformed
            [iii|any [ #{verKeyH1}, #{verKeyH2}, active_from a]|]

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
verKeyH1 = "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"

verKeyH2 :: String
verKeyH2 = "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp"

verKeyH3 :: String
verKeyH3 = "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"

verKeyH4 :: String
verKeyH4 = "addr_shared_vkh1fee6yrlnczhfp77ftunc6snjrv0hv0s92qj2pe47dt4hz8ajp6a"
