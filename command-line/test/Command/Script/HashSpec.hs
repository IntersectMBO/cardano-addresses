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

        specScriptHashProper "drep_script13hu7a632tuntrlyjkazzczrzzt7dx6e8v3rc25jd9zcgv68nx9r"
            [iii|all [ #{verKeyH5} ]|]

        specScriptHashProper "drep_script13hu7a632tuntrlyjkazzczrzzt7dx6e8v3rc25jd9zcgv68nx9r"
            [iii|all [ #{verKey5} ]|]

        specScriptHashProper "drep_script13hu7a632tuntrlyjkazzczrzzt7dx6e8v3rc25jd9zcgv68nx9r"
            [iii|all [ #{xVerKey5} ]|]

        specScriptHashProper "drep_script16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v29l95rg"
            [iii|all [ #{verKeyH5}, active_from 5001]|]

        specScriptHashProper "drep_script16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v29l95rg"
            [iii|all [ #{verKeyH5}, active_from 5001 ]|]

        specScriptHashProper "drep_script16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v29l95rg"
            [iii|all [ #{verKeyH5}, active_from 5001 ]|]

        specScriptHashProper "drep_script14edv7pg3y4wkglyykvvy5t2j906ld3dhdwvf7jda8qaa63d5kf4"
            [iii|any [ #{verKeyH5}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "drep_script14edv7pg3y4wkglyykvvy5t2j906ld3dhdwvf7jda8qaa63d5kf4"
            [iii|any [ #{verKey5}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "drep_script14edv7pg3y4wkglyykvvy5t2j906ld3dhdwvf7jda8qaa63d5kf4"
            [iii|any [ #{xVerKey5}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold_script16e9ypar36jqppdpff86u578w5lvpuf55htkk5cryvj54wd8s4jn"
            [iii|all [ #{verKeyH6} ]|]

        specScriptHashProper "cc_cold_script16e9ypar36jqppdpff86u578w5lvpuf55htkk5cryvj54wd8s4jn"
            [iii|all [ #{verKey6} ]|]

        specScriptHashProper "cc_cold_script16e9ypar36jqppdpff86u578w5lvpuf55htkk5cryvj54wd8s4jn"
            [iii|all [ #{xVerKey6} ]|]

        specScriptHashProper "cc_cold_script14ehj5f64f40xju0086fnunctulkh46mq7munm7upe4hpcwpcatv"
            [iii|all [ #{verKeyH6}, active_from 5001]|]

        specScriptHashProper "cc_cold_script14ehj5f64f40xju0086fnunctulkh46mq7munm7upe4hpcwpcatv"
            [iii|all [ #{verKeyH6}, active_from 5001 ]|]

        specScriptHashProper "cc_cold_script14ehj5f64f40xju0086fnunctulkh46mq7munm7upe4hpcwpcatv"
            [iii|all [ #{verKeyH6}, active_from 5001 ]|]

        specScriptHashProper "cc_cold_script1zxwzpnk0ah7m5ptjjtmkhvgs4736k3e0ns66shd0fy33vdauq3j"
            [iii|any [ #{verKeyH6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold_script1zxwzpnk0ah7m5ptjjtmkhvgs4736k3e0ns66shd0fy33vdauq3j"
            [iii|any [ #{verKey6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold_script1zxwzpnk0ah7m5ptjjtmkhvgs4736k3e0ns66shd0fy33vdauq3j"
            [iii|any [ #{xVerKey6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot_script10fpewpnxmmc7x08nc5yepdyafa8arcaugf3pa37l3qmn2xu0u3c"
            [iii|all [ #{verKeyH7} ]|]

        specScriptHashProper "cc_hot_script10fpewpnxmmc7x08nc5yepdyafa8arcaugf3pa37l3qmn2xu0u3c"
            [iii|all [ #{verKey7} ]|]

        specScriptHashProper "cc_hot_script10fpewpnxmmc7x08nc5yepdyafa8arcaugf3pa37l3qmn2xu0u3c"
            [iii|all [ #{xVerKey7} ]|]

        specScriptHashProper "cc_hot_script16fayy2wf9myfvxmtl5e2suuqmnhy5zx80vxkezen7xqwskncf40"
            [iii|all [ #{verKeyH7}, active_from 5001]|]

        specScriptHashProper "cc_hot_script16fayy2wf9myfvxmtl5e2suuqmnhy5zx80vxkezen7xqwskncf40"
            [iii|all [ #{verKeyH7}, active_from 5001 ]|]

        specScriptHashProper "cc_hot_script16fayy2wf9myfvxmtl5e2suuqmnhy5zx80vxkezen7xqwskncf40"
            [iii|all [ #{verKeyH7}, active_from 5001 ]|]

        specScriptHashProper "cc_hot_script1vts8nrrsxmlntp3v7sh5u7k6qmmlkkmyv5uspq4xjxlpg6u229p"
            [iii|any [ #{verKeyH7}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot_script1vts8nrrsxmlntp3v7sh5u7k6qmmlkkmyv5uspq4xjxlpg6u229p"
            [iii|any [ #{verKey7}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot_script1vts8nrrsxmlntp3v7sh5u7k6qmmlkkmyv5uspq4xjxlpg6u229p"
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

verKeyH5 :: String
verKeyH5 = "drep15k6929drl7xt0spvudgcxndryn4kmlzpk4meed0xhqe25nle07s"

verKey5 :: String
verKey5 = "drep_vk17axh4sc9zwkpsft3tlgpjemfwc0u5mnld80r85zw7zdqcst6w54sdv4a4e"

xVerKey5 :: String
xVerKey5 = "drep_xvk17axh4sc9zwkpsft3tlgpjemfwc0u5mnld80r85zw7zdqcst6w543mpq3q2vkjy3nw8x7n8asw4es78dyl4q7u7kwlwn7yy0sugxfrjs6z25qe"

verKeyH6 :: String
verKeyH6 = "cc_cold1lmaet9hdvu9d9jvh34u0un4ndw3yewaq5ch6fnwsctw02xxwylj"

verKey6 :: String
verKey6 = "cc_cold_vk149up407pvp9p36lldlp4qckqqzn6vm7u5yerwy8d8rqalse3t04q7qsvwl"

xVerKey6 :: String
xVerKey6 = "cc_cold_xvk149up407pvp9p36lldlp4qckqqzn6vm7u5yerwy8d8rqalse3t04vvqvk3e6l7vzjl7n8ttk646jflumvkgcrdhcstc5wr5etg5n7dnc8nqv5d"

verKeyH7 :: String
verKeyH7 = "cc_hot17mffcrm3vnfhvyxt7ea3y65e804jfgrk6pjn78aqd9vg7xpq8dv"

verKey7 :: String
verKey7 = "cc_hot_vk10y48lq72hypxraew74lwjjn9e2dscuwphckglh2nrrpkgweqk5hschnzv5"

xVerKey7 :: String
xVerKey7 = "cc_hot_xvk10y48lq72hypxraew74lwjjn9e2dscuwphckglh2nrrpkgweqk5h4fplggm56wz9jw6qadq6l5tdvj6qs3v7ggh3hjkt5j8ntga42pvs5rvh0a"
