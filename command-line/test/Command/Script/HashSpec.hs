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

import qualified Debug.Trace as TR

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

        specScriptHashProper "drep1ywxlnmh29f0jdv0uj2m5gtqgvgf0e5mtyajy0p2jf55tppszrkgvs"
            [iii|all [ #{verKeyH5} ]|]

        specScriptHashProper "drep1ywxlnmh29f0jdv0uj2m5gtqgvgf0e5mtyajy0p2jf55tppszrkgvs"
            [iii|all [ #{verKey5} ]|]

        specScriptHashProper "drep1ywxlnmh29f0jdv0uj2m5gtqgvgf0e5mtyajy0p2jf55tppszrkgvs"
            [iii|all [ #{xVerKey5} ]|]

        specScriptHashProper "drep_script16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v29l95rg"
            [iii|all [ #{verKeyH5}, active_from 5001]|]

        specScriptHashProper "drep_script16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v29l95rg"
            [iii|all [ #{verKeyH5}, active_from 5001 ]|]

        specScriptHashProper "drep_script16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v29l95rg"
            [iii|all [ #{verKeyH5}, active_from 5001 ]|]

        specScriptHashProper "drep1ywh94nc9zyj46erusje3sj3d2g4ltak9ka4e386fh5urhhga37qxs"
            [iii|any [ #{verKeyH5}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "drep1ywh94nc9zyj46erusje3sj3d2g4ltak9ka4e386fh5urhhga37qxs"
            [iii|any [ #{verKey5}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "drep1ywh94nc9zyj46erusje3sj3d2g4ltak9ka4e386fh5urhhga37qxs"
            [iii|any [ #{xVerKey5}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold1z0ty5s85w82gqy9599yltjnca6nas83xjjaw66nqv3j2j4cxpgzfw"
            [iii|all [ #{verKeyH6} ]|]

        specScriptHashProper "cc_cold1z0ty5s85w82gqy9599yltjnca6nas83xjjaw66nqv3j2j4cxpgzfw"
            [iii|all [ #{verKey6} ]|]

        specScriptHashProper "cc_cold1z0ty5s85w82gqy9599yltjnca6nas83xjjaw66nqv3j2j4cxpgzfw"
            [iii|all [ #{xVerKey6} ]|]

        specScriptHashProper "cc_cold1z0dnhem5shnestg4lj59k4845vtnugav0h27w6t42fcqh2q24vdtr"
            [iii|all [ #{verKeyH6}, active_from 5001]|]

        specScriptHashProper "cc_cold1z0dnhem5shnestg4lj59k4845vtnugav0h27w6t42fcqh2q24vdtr"
            [iii|all [ #{verKeyH6}, active_from 5001 ]|]

        specScriptHashProper "cc_cold1z0dnhem5shnestg4lj59k4845vtnugav0h27w6t42fcqh2q24vdtr"
            [iii|all [ #{verKeyH6}, active_from 5001 ]|]

        specScriptHashProper "cc_cold1z0ngc02z5q289dekhkggcmzfk3v0tsega8mmual530z0d0c48j4kl"
            [iii|any [ #{verKeyH6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold1z0ngc02z5q289dekhkggcmzfk3v0tsega8mmual530z0d0c48j4kl"
            [iii|any [ #{verKey6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold1z0ngc02z5q289dekhkggcmzfk3v0tsega8mmual530z0d0c48j4kl"
            [iii|any [ #{xVerKey6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot1qday89cxvm00rceu70zsny95n485l50rh3pxy8k8m7yrwdgrehyfq"
            [iii|all [ #{verKeyH7} ]|]

        specScriptHashProper "cc_hot1qday89cxvm00rceu70zsny95n485l50rh3pxy8k8m7yrwdgrehyfq"
            [iii|all [ #{verKey7} ]|]

        specScriptHashProper "cc_hot1qday89cxvm00rceu70zsny95n485l50rh3pxy8k8m7yrwdgrehyfq"
            [iii|all [ #{xVerKey7} ]|]

        specScriptHashProper "cc_hot_script16fayy2wf9myfvxmtl5e2suuqmnhy5zx80vxkezen7xqwskncf40"
            [iii|all [ #{verKeyH7}, active_from 5001]|]

        specScriptHashProper "cc_hot_script16fayy2wf9myfvxmtl5e2suuqmnhy5zx80vxkezen7xqwskncf40"
            [iii|all [ #{verKeyH7}, active_from 5001 ]|]

        specScriptHashProper "cc_hot_script16fayy2wf9myfvxmtl5e2suuqmnhy5zx80vxkezen7xqwskncf40"
            [iii|all [ #{verKeyH7}, active_from 5001 ]|]

        specScriptHashProper "cc_hot1qd3wq7vvwqm07dvx9n6z7nn6mgr0076mv3jnjqyz56gmu9qaj7nrc"
            [iii|any [ #{verKeyH7}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot1qd3wq7vvwqm07dvx9n6z7nn6mgr0076mv3jnjqyz56gmu9qaj7nrc"
            [iii|any [ #{verKey7}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot1qd3wq7vvwqm07dvx9n6z7nn6mgr0076mv3jnjqyz56gmu9qaj7nrc"
            [iii|any [ #{xVerKey7}, all [ active_from 5001, active_until 6001]]|]

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
    TR.trace("out: "<> show out) $ out `shouldBe` expected

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

verKeyH5 :: String
verKeyH5 = "drep1y0je8qacjy4yua6esh86rdjqpdactf8wph05gdd72u46axca7nz6m"

verKey5 :: String
verKey5 = "drep_vk17axh4sc9zwkpsft3tlgpjemfwc0u5mnld80r85zw7zdqcst6w54sdv4a4e"

xVerKey5 :: String
xVerKey5 = "drep_xvk17axh4sc9zwkpsft3tlgpjemfwc0u5mnld80r85zw7zdqcst6w543mpq3q2vkjy3nw8x7n8asw4es78dyl4q7u7kwlwn7yy0sugxfrjs6z25qe"

verKeyH6 :: String
verKeyH6 = "cc_cold1ztje8qacjy4yua6esh86rdjqpdactf8wph05gdd72u46axcey99nc"

verKey6 :: String
verKey6 = "cc_cold_vk149up407pvp9p36lldlp4qckqqzn6vm7u5yerwy8d8rqalse3t04q7qsvwl"

xVerKey6 :: String
xVerKey6 = "cc_cold_xvk149up407pvp9p36lldlp4qckqqzn6vm7u5yerwy8d8rqalse3t04vvqvk3e6l7vzjl7n8ttk646jflumvkgcrdhcstc5wr5etg5n7dnc8nqv5d"

verKeyH7 :: String
verKeyH7 = "cc_hot1qtje8qacjy4yua6esh86rdjqpdactf8wph05gdd72u46axc0c3ef3"

verKey7 :: String
verKey7 = "cc_hot_vk10y48lq72hypxraew74lwjjn9e2dscuwphckglh2nrrpkgweqk5hschnzv5"

xVerKey7 :: String
xVerKey7 = "cc_hot_xvk10y48lq72hypxraew74lwjjn9e2dscuwphckglh2nrrpkgweqk5h4fplggm56wz9jw6qadq6l5tdvj6qs3v7ggh3hjkt5j8ntga42pvs5rvh0a"
