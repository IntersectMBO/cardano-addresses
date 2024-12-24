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

        specScriptHashProper "drep1ydv8xcdxwpy7zrf805yenpz7cdnap4c87v7zewvmdjyyygq44jy4j"
            [iii|all [ #{verKeyH5} ]|]

        specScriptHashProper "drep1ydv8xcdxwpy7zrf805yenpz7cdnap4c87v7zewvmdjyyygq44jy4j"
            [iii|all [ #{verKey5} ]|]

        specScriptHashProper "drep1ydv8xcdxwpy7zrf805yenpz7cdnap4c87v7zewvmdjyyygq44jy4j"
            [iii|all [ #{xVerKey5} ]|]

        specScriptHashProper "drep1y0fu7a88hnzxa3y93hqpe9ku34tp80224gv8ftzcqwzm5gsp5y9q7"
            [iii|all [ #{verKeyH5}, active_from 5001]|]

        specScriptHashProper "drep1y0fu7a88hnzxa3y93hqpe9ku34tp80224gv8ftzcqwzm5gsp5y9q7"
            [iii|all [ #{verKey5}, active_from 5001 ]|]

        specScriptHashProper "drep1y0fu7a88hnzxa3y93hqpe9ku34tp80224gv8ftzcqwzm5gsp5y9q7"
            [iii|all [ #{xVerKey5}, active_from 5001 ]|]

        specScriptHashProper "drep1y07mghw6eaqmut4gxxd2q8cy0eukdhyc0jwu3yrr4lfrzcs2em3aw"
            [iii|any [ #{verKeyH5}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "drep1y07mghw6eaqmut4gxxd2q8cy0eukdhyc0jwu3yrr4lfrzcs2em3aw"
            [iii|any [ #{verKey5}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "drep1y07mghw6eaqmut4gxxd2q8cy0eukdhyc0jwu3yrr4lfrzcs2em3aw"
            [iii|any [ #{xVerKey5}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold1zw95xrvvk7kzhkjf77k7sxya4kkjxpx8mgmcnvm9tgqeswcmdjf5r"
            [iii|all [ #{verKeyH6} ]|]

        specScriptHashProper "cc_cold1zw95xrvvk7kzhkjf77k7sxya4kkjxpx8mgmcnvm9tgqeswcmdjf5r"
            [iii|all [ #{verKey6} ]|]

        specScriptHashProper "cc_cold1zw95xrvvk7kzhkjf77k7sxya4kkjxpx8mgmcnvm9tgqeswcmdjf5r"
            [iii|all [ #{xVerKey6} ]|]

        specScriptHashProper "cc_cold1zddamn0j6gs6h228yuag0ld40aqwfa7zr2vk9y43ptpj2xse44ze0"
            [iii|all [ #{verKeyH6}, active_from 5001]|]

        specScriptHashProper "cc_cold1zddamn0j6gs6h228yuag0ld40aqwfa7zr2vk9y43ptpj2xse44ze0"
            [iii|all [ #{verKeyH6}, active_from 5001 ]|]

        specScriptHashProper "cc_cold1zddamn0j6gs6h228yuag0ld40aqwfa7zr2vk9y43ptpj2xse44ze0"
            [iii|all [ #{verKeyH6}, active_from 5001 ]|]

        specScriptHashProper "cc_cold1z0jnmwavkmnzyszp9gp7qalsu3t0za3ydasv9ajf7xl4vyqzccwty"
            [iii|any [ #{verKeyH6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold1z0jnmwavkmnzyszp9gp7qalsu3t0za3ydasv9ajf7xl4vyqzccwty"
            [iii|any [ #{verKey6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold1z0jnmwavkmnzyszp9gp7qalsu3t0za3ydasv9ajf7xl4vyqzccwty"
            [iii|any [ #{xVerKey6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot1qwdtt2x7kcjjuas5xxhgvzh54527pjkmps0nrx8xu5dzmngyy6q5g"
            [iii|all [ #{verKeyH7} ]|]

        specScriptHashProper "cc_hot1qwdtt2x7kcjjuas5xxhgvzh54527pjkmps0nrx8xu5dzmngyy6q5g"
            [iii|all [ #{verKey7} ]|]

        specScriptHashProper "cc_hot1qwdtt2x7kcjjuas5xxhgvzh54527pjkmps0nrx8xu5dzmngyy6q5g"
            [iii|all [ #{xVerKey7} ]|]

        specScriptHashProper "cc_hot1qvljt8yyl4r0fzaxkyqkd9q3qwchxcra0mh8tec4rrnpqhg7d25xr"
            [iii|all [ #{verKeyH7}, active_from 5001]|]

        specScriptHashProper "cc_hot1qvljt8yyl4r0fzaxkyqkd9q3qwchxcra0mh8tec4rrnpqhg7d25xr"
            [iii|all [ #{verKey7}, active_from 5001 ]|]

        specScriptHashProper "cc_hot1qvljt8yyl4r0fzaxkyqkd9q3qwchxcra0mh8tec4rrnpqhg7d25xr"
            [iii|all [ #{xVerKey7}, active_from 5001 ]|]

        specScriptHashProper "cc_hot1qw4u9l6jvl26g09844ny3hev7dpqvuxv6ney5q33zwu0qpquwapzf"
            [iii|any [ #{verKeyH7}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot1qw4u9l6jvl26g09844ny3hev7dpqvuxv6ney5q33zwu0qpquwapzf"
            [iii|any [ #{verKey7}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot1qw4u9l6jvl26g09844ny3hev7dpqvuxv6ney5q33zwu0qpquwapzf"
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
    out <- cli ["script", "hash", script, "--with-byte"] ""
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
verKeyH5 = "drep1y2qxs6anwflkqfvpxzg30k8pesr6gy8pgdmd8zppq8ffnksapjznm"

verKey5 :: String
verKey5 = "drep_vk1mg7xae48d7z4nntd35tey0jmclxaavwmk3kw2lkkt07p3s3x3yysra6588"

xVerKey5 :: String
xVerKey5 = "drep_xvk1mg7xae48d7z4nntd35tey0jmclxaavwmk3kw2lkkt07p3s3x3yy45805manx2kj2neg40kfpy9em36vnkjfm4fw09k66837unrvd70qq8ewzf"

verKeyH6 :: String
verKeyH6 = "cc_cold1zfhc368fgydj0hx38qal2yeh8596q59vf4z2cpk4k6yp4hqy3mpsx"

verKey6 :: String
verKey6 = "cc_cold_vk1dg8d5du0v4ukqkfgset50xncudhwlfzz2p6epv096x0ndl8jsgzqmwj2x5"

xVerKey6 :: String
xVerKey6 = "cc_cold_xvk1dg8d5du0v4ukqkfgset50xncudhwlfzz2p6epv096x0ndl8jsgzzdqzpe9yw37u7mu7xaenhv7242990ps3sn8jcg52yx7n3fuke9kst5t2py"

verKeyH7 :: String
verKeyH7 = "cc_hot1qg6ck5scr3y0wjzf9jjxjl7wcslpy4yfzjesxyw5qg4knyg9ckh0d"

verKey7 :: String
verKey7 = "cc_hot_vk1a5q4r34xzm0r6y728d4gmrl7jvrfuh7r022k7wh5mzwmyg7d7l3s3fzqkv"

xVerKey7 :: String
xVerKey7 = "cc_hot_xvk1a5q4r34xzm0r6y728d4gmrl7jvrfuh7r022k7wh5mzwmyg7d7l3hjwwaw54qwj0rn084enysj8ha2vwg2wd7umksf4tcnskaj8xr4tcempwly"
