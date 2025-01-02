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

        specScriptHashProper "drep1ywxlnmh29f0jdv0uj2m5gtqgvgf0e5mtyajy0p2jf55tppszrkgvs"
            [iii|all [ #{verKeyH5} ]|]

        specScriptHashWithoutByteProper "drep_script13hu7a632tuntrlyjkazzczrzzt7dx6e8v3rc25jd9zcgv68nx9r"
            [iii|all [ #{verKeyH5} ]|]

        specScriptHashProper "drep1ywxlnmh29f0jdv0uj2m5gtqgvgf0e5mtyajy0p2jf55tppszrkgvs"
            [iii|all [ #{verKey5} ]|]

        specScriptHashWithoutByteProper "drep_script13hu7a632tuntrlyjkazzczrzzt7dx6e8v3rc25jd9zcgv68nx9r"
            [iii|all [ #{verKey5} ]|]

        specScriptHashProper "drep1ywxlnmh29f0jdv0uj2m5gtqgvgf0e5mtyajy0p2jf55tppszrkgvs"
            [iii|all [ #{xVerKey5} ]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#script-1-hash-drep-script-hash
        specScriptHashWithoutByteProper "drep_script13hu7a632tuntrlyjkazzczrzzt7dx6e8v3rc25jd9zcgv68nx9r"
            [iii|all [ #{xVerKey5} ]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-script-1-hash-appended-with-23-hex-encoded-byte-drep-script-hash-credential
        specScriptHashProper "drep1y0gx2ufxm0cvzdd8yfxerjsx3adlw6d0d503mu9uu5tsa3gtkvwpe"
            [iii|all [ #{verKeyH5}, active_from 5001]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-script-1-hash-appended-with-23-hex-encoded-byte-drep-script-hash-credential
        specScriptHashProper "drep1y0gx2ufxm0cvzdd8yfxerjsx3adlw6d0d503mu9uu5tsa3gtkvwpe"
            [iii|all [ #{verKeyH5Depr}, active_from 5001]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-script-1-hash-appended-with-23-hex-encoded-byte-drep-script-hash-credential
        specScriptHashProper "drep1y0gx2ufxm0cvzdd8yfxerjsx3adlw6d0d503mu9uu5tsa3gtkvwpe"
            [iii|all [ #{verKeyH5Cred}, active_from 5001]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#script-1-hash-drep-script-hash
        specScriptHashWithoutByteProper "drep_script16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v29l95rg"
            [iii|all [ #{verKeyH5}, active_from 5001]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#script-1-hash-drep-script-hash
        specScriptHashWithoutByteProper "drep_script16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v29l95rg"
            [iii|all [ #{verKeyH5Depr}, active_from 5001]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#script-1-hash-drep-script-hash
        specScriptHashWithoutByteProper "drep_script16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v29l95rg"
            [iii|all [ #{verKeyH5Cred}, active_from 5001]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-script-1-hash-appended-with-23-hex-encoded-byte-drep-script-hash-credential
        specScriptHashProper "drep1y0gx2ufxm0cvzdd8yfxerjsx3adlw6d0d503mu9uu5tsa3gtkvwpe"
            [iii|all [ #{verKey5}, active_from 5001 ]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#script-1-hash-drep-script-hash
        specScriptHashWithoutByteProper "drep_script16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v29l95rg"
            [iii|all [ #{verKey5}, active_from 5001 ]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-script-1-hash-appended-with-23-hex-encoded-byte-drep-script-hash-credential
        specScriptHashProper "drep1y0gx2ufxm0cvzdd8yfxerjsx3adlw6d0d503mu9uu5tsa3gtkvwpe"
            [iii|all [ #{xVerKey5}, active_from 5001 ]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#script-1-hash-drep-script-hash
        specScriptHashWithoutByteProper "drep_script16pjhzfkm7rqntfezfkgu5p50t0mkntmdruwlp089zu8v29l95rg"
            [iii|all [ #{xVerKey5}, active_from 5001 ]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-script-2-hash-appended-with-23-hex-encoded-byte-drep-script-hash-credential
        specScriptHashProper "drep1ywh94nc9zyj46erusje3sj3d2g4ltak9ka4e386fh5urhhga37qxs"
            [iii|any [ #{verKeyH5}, all [ active_from 5001, active_until 6001]]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-script-2-hash-appended-with-23-hex-encoded-byte-drep-script-hash-credential
        specScriptHashProper "drep1ywh94nc9zyj46erusje3sj3d2g4ltak9ka4e386fh5urhhga37qxs"
            [iii|any [ #{verKeyH5Depr}, all [ active_from 5001, active_until 6001]]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-script-2-hash-appended-with-23-hex-encoded-byte-drep-script-hash-credential
        specScriptHashProper "drep1ywh94nc9zyj46erusje3sj3d2g4ltak9ka4e386fh5urhhga37qxs"
            [iii|any [ #{verKeyH5Cred}, all [ active_from 5001, active_until 6001]]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#script-2-hash-drep-script-hash
        specScriptHashWithoutByteProper "drep_script14edv7pg3y4wkglyykvvy5t2j906ld3dhdwvf7jda8qaa63d5kf4"
            [iii|any [ #{verKeyH5}, all [ active_from 5001, active_until 6001]]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#script-2-hash-drep-script-hash
        specScriptHashWithoutByteProper "drep_script14edv7pg3y4wkglyykvvy5t2j906ld3dhdwvf7jda8qaa63d5kf4"
            [iii|any [ #{verKeyH5Depr}, all [ active_from 5001, active_until 6001]]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#script-2-hash-drep-script-hash
        specScriptHashWithoutByteProper "drep_script14edv7pg3y4wkglyykvvy5t2j906ld3dhdwvf7jda8qaa63d5kf4"
            [iii|any [ #{verKeyH5Cred}, all [ active_from 5001, active_until 6001]]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-script-2-hash-appended-with-23-hex-encoded-byte-drep-script-hash-credential
        specScriptHashProper "drep1ywh94nc9zyj46erusje3sj3d2g4ltak9ka4e386fh5urhhga37qxs"
            [iii|any [ #{verKey5}, all [ active_from 5001, active_until 6001]]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#script-2-hash-drep-script-hash
        specScriptHashWithoutByteProper "drep_script14edv7pg3y4wkglyykvvy5t2j906ld3dhdwvf7jda8qaa63d5kf4"
            [iii|any [ #{verKey5}, all [ active_from 5001, active_until 6001]]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-script-2-hash-appended-with-23-hex-encoded-byte-drep-script-hash-credential
        specScriptHashProper "drep1ywh94nc9zyj46erusje3sj3d2g4ltak9ka4e386fh5urhhga37qxs"
            [iii|any [ #{xVerKey5}, all [ active_from 5001, active_until 6001]]|]

        -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#script-2-hash-drep-script-hash
        specScriptHashWithoutByteProper "drep_script14edv7pg3y4wkglyykvvy5t2j906ld3dhdwvf7jda8qaa63d5kf4"
            [iii|any [ #{xVerKey5}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold1z0ty5s85w82gqy9599yltjnca6nas83xjjaw66nqv3j2j4cxpgzfw"
            [iii|all [ #{verKeyH6} ]|]

        specScriptHashWithoutByteProper "cc_cold_script16e9ypar36jqppdpff86u578w5lvpuf55htkk5cryvj54wd8s4jn"
            [iii|all [ #{verKeyH6} ]|]

        specScriptHashProper "cc_cold1z0ty5s85w82gqy9599yltjnca6nas83xjjaw66nqv3j2j4cxpgzfw"
            [iii|all [ #{verKey6} ]|]

        specScriptHashWithoutByteProper "cc_cold_script16e9ypar36jqppdpff86u578w5lvpuf55htkk5cryvj54wd8s4jn"
            [iii|all [ #{verKey6} ]|]

        specScriptHashProper "cc_cold1z0ty5s85w82gqy9599yltjnca6nas83xjjaw66nqv3j2j4cxpgzfw"
            [iii|all [ #{xVerKey6} ]|]

        specScriptHashWithoutByteProper "cc_cold_script16e9ypar36jqppdpff86u578w5lvpuf55htkk5cryvj54wd8s4jn"
            [iii|all [ #{xVerKey6} ]|]

        specScriptHashProper "cc_cold1zwhx723824x4u6t3aulfx0j0p0n767htvrm0j00ms8xku8q30p2xd"
            [iii|all [ #{verKeyH6}, active_from 5001]|]

        specScriptHashWithoutByteProper "cc_cold_script14ehj5f64f40xju0086fnunctulkh46mq7munm7upe4hpcwpcatv"
            [iii|all [ #{verKeyH6}, active_from 5001]|]

        specScriptHashProper "cc_cold1zwhx723824x4u6t3aulfx0j0p0n767htvrm0j00ms8xku8q30p2xd"
            [iii|all [ #{verKeyH6Depr}, active_from 5001 ]|]

        specScriptHashWithoutByteProper "cc_cold_script14ehj5f64f40xju0086fnunctulkh46mq7munm7upe4hpcwpcatv"
            [iii|all [ #{verKeyH6Depr}, active_from 5001 ]|]

        specScriptHashProper "cc_cold1zwhx723824x4u6t3aulfx0j0p0n767htvrm0j00ms8xku8q30p2xd"
            [iii|all [ #{verKeyH6Cred}, active_from 5001 ]|]

        specScriptHashWithoutByteProper "cc_cold_script14ehj5f64f40xju0086fnunctulkh46mq7munm7upe4hpcwpcatv"
            [iii|all [ #{verKeyH6Cred}, active_from 5001 ]|]

        specScriptHashProper "cc_cold1zwhx723824x4u6t3aulfx0j0p0n767htvrm0j00ms8xku8q30p2xd"
            [iii|all [ #{verKey6}, active_from 5001]|]

        specScriptHashWithoutByteProper "cc_cold_script14ehj5f64f40xju0086fnunctulkh46mq7munm7upe4hpcwpcatv"
            [iii|all [ #{verKey6}, active_from 5001]|]

        specScriptHashProper "cc_cold1zwhx723824x4u6t3aulfx0j0p0n767htvrm0j00ms8xku8q30p2xd"
            [iii|all [ #{xVerKey6}, active_from 5001]|]

        specScriptHashWithoutByteProper "cc_cold_script14ehj5f64f40xju0086fnunctulkh46mq7munm7upe4hpcwpcatv"
            [iii|all [ #{xVerKey6}, active_from 5001]|]

        specScriptHashProper "cc_cold1zvgecgxwelklmws9w2f0w6a3zzh6826897wrt2za4ayjx9swtgkr6"
            [iii|any [ #{verKeyH6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashWithoutByteProper "cc_cold_script1zxwzpnk0ah7m5ptjjtmkhvgs4736k3e0ns66shd0fy33vdauq3j"
            [iii|any [ #{verKeyH6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold1zvgecgxwelklmws9w2f0w6a3zzh6826897wrt2za4ayjx9swtgkr6"
            [iii|any [ #{verKeyH6Depr}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashWithoutByteProper "cc_cold_script1zxwzpnk0ah7m5ptjjtmkhvgs4736k3e0ns66shd0fy33vdauq3j"
            [iii|any [ #{verKeyH6Depr}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold1zvgecgxwelklmws9w2f0w6a3zzh6826897wrt2za4ayjx9swtgkr6"
            [iii|any [ #{verKeyH6Cred}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashWithoutByteProper "cc_cold_script1zxwzpnk0ah7m5ptjjtmkhvgs4736k3e0ns66shd0fy33vdauq3j"
            [iii|any [ #{verKeyH6Cred}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold1zvgecgxwelklmws9w2f0w6a3zzh6826897wrt2za4ayjx9swtgkr6"
            [iii|any [ #{verKey6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashWithoutByteProper "cc_cold_script1zxwzpnk0ah7m5ptjjtmkhvgs4736k3e0ns66shd0fy33vdauq3j"
            [iii|any [ #{verKey6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_cold1zvgecgxwelklmws9w2f0w6a3zzh6826897wrt2za4ayjx9swtgkr6"
            [iii|any [ #{xVerKey6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashWithoutByteProper "cc_cold_script1zxwzpnk0ah7m5ptjjtmkhvgs4736k3e0ns66shd0fy33vdauq3j"
            [iii|any [ #{xVerKey6}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot1qwdtt2x7kcjjuas5xxhgvzh54527pjkmps0nrx8xu5dzmngyy6q5g"
            [iii|all [ #{verKeyH7} ]|]

        specScriptHashWithoutByteProper "cc_hot_script1n2663h4ky5h8v9p346rq4a9dzhsv4kcvruce3eh9rgku6xg0666"
            [iii|all [ #{verKeyH7} ]|]

        specScriptHashProper "cc_hot1qwdtt2x7kcjjuas5xxhgvzh54527pjkmps0nrx8xu5dzmngyy6q5g"
            [iii|all [ #{verKey7} ]|]

        specScriptHashWithoutByteProper "cc_hot_script1n2663h4ky5h8v9p346rq4a9dzhsv4kcvruce3eh9rgku6xg0666"
            [iii|all [ #{verKey7} ]|]

        specScriptHashProper "cc_hot1qwdtt2x7kcjjuas5xxhgvzh54527pjkmps0nrx8xu5dzmngyy6q5g"
            [iii|all [ #{xVerKey7} ]|]

        specScriptHashWithoutByteProper "cc_hot_script1n2663h4ky5h8v9p346rq4a9dzhsv4kcvruce3eh9rgku6xg0666"
            [iii|all [ #{xVerKey7} ]|]

        specScriptHashProper "cc_hot1qvljt8yyl4r0fzaxkyqkd9q3qwchxcra0mh8tec4rrnpqhg7d25xr"
            [iii|all [ #{verKeyH7}, active_from 5001]|]

        specScriptHashWithoutByteProper "cc_hot_script18ujeep8agm6ghf43q9nfgygrk9ekqlt7ae67w9gcucg968m2g5a"
            [iii|all [ #{verKeyH7}, active_from 5001]|]

        specScriptHashProper "cc_hot1qvljt8yyl4r0fzaxkyqkd9q3qwchxcra0mh8tec4rrnpqhg7d25xr"
            [iii|all [ #{verKey7}, active_from 5001 ]|]

        specScriptHashWithoutByteProper "cc_hot_script18ujeep8agm6ghf43q9nfgygrk9ekqlt7ae67w9gcucg968m2g5a"
            [iii|all [ #{verKey7}, active_from 5001 ]|]

        specScriptHashProper "cc_hot1qvljt8yyl4r0fzaxkyqkd9q3qwchxcra0mh8tec4rrnpqhg7d25xr"
            [iii|all [ #{xVerKey7}, active_from 5001 ]|]

        specScriptHashWithoutByteProper "cc_hot_script18ujeep8agm6ghf43q9nfgygrk9ekqlt7ae67w9gcucg968m2g5a"
            [iii|all [ #{xVerKey7}, active_from 5001 ]|]

        specScriptHashProper "cc_hot1qw4u9l6jvl26g09844ny3hev7dpqvuxv6ney5q33zwu0qpquwapzf"
            [iii|any [ #{verKeyH7}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashWithoutByteProper "cc_hot_script140p075n86kjrefadveyd7t8nggr8pnx57f9qyvgnhrcqg6ekh2c"
            [iii|any [ #{verKeyH7}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot1qw4u9l6jvl26g09844ny3hev7dpqvuxv6ney5q33zwu0qpquwapzf"
            [iii|any [ #{verKey7}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashWithoutByteProper "cc_hot_script140p075n86kjrefadveyd7t8nggr8pnx57f9qyvgnhrcqg6ekh2c"
            [iii|any [ #{verKey7}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashProper "cc_hot1qw4u9l6jvl26g09844ny3hev7dpqvuxv6ney5q33zwu0qpquwapzf"
            [iii|any [ #{xVerKey7}, all [ active_from 5001, active_until 6001]]|]

        specScriptHashWithoutByteProper "cc_hot_script140p075n86kjrefadveyd7t8nggr8pnx57f9qyvgnhrcqg6ekh2c"
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

specScriptHashWithoutByteProper :: String -> String -> SpecWith ()
specScriptHashWithoutByteProper expected script = it (script <> " => " <> expected) $ do
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

-- Keys in accordance to https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md

-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#verification-key-hash-drep-vkh
-- Verification key hash (DRep VKH)
verKeyH5 :: String
verKeyH5 = "drep_vkh15k6929drl7xt0spvudgcxndryn4kmlzpk4meed0xhqe254czjh2"

-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#deprecated-verification-key-hash-drep-id
-- [DEPRECATED] Verification key hash (DRep ID)
verKeyH5Depr :: String
verKeyH5Depr = "drep15k6929drl7xt0spvudgcxndryn4kmlzpk4meed0xhqe25nle07s"

-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-compliant-verification-key-hash-appended-with--22-hex-encoded-byte-drep-key-hash-credential
-- [CIP-0129 compliant] Verification key hash appended with '22' hex-encoded byte (DRep key hash credential)
verKeyH5Cred :: String
verKeyH5Cred = "drep1y2jmg4g450lced7q9n34rq6d5vjwkm0ugx6h0894u6ur92s9txn3a"

-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#drep-verification-key
-- DRep verification key
verKey5 :: String
verKey5 = "drep_vk17axh4sc9zwkpsft3tlgpjemfwc0u5mnld80r85zw7zdqcst6w54sdv4a4e"

-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#drep-extended-verification-key
-- DRep extended verification key
xVerKey5 :: String
xVerKey5 = "drep_xvk17axh4sc9zwkpsft3tlgpjemfwc0u5mnld80r85zw7zdqcst6w543mpq3q2vkjy3nw8x7n8asw4es78dyl4q7u7kwlwn7yy0sugxfrjs6z25qe"

-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#constitutional-committee-cold-verification-key-hash-constitutional-committee-cold-vkh
-- Constitutional Committee Cold Verification key hash (Constitutional Committee Cold VKH)
verKeyH6 :: String
verKeyH6 = "cc_cold_vkh1lmaet9hdvu9d9jvh34u0un4ndw3yewaq5ch6fnwsctw0243cw47"

-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#deprecated-constitutional-committee-cold-verification-key-hash
-- [DEPRECATED] Constitutional Committee Cold Verification Key Hash
verKeyH6Depr :: String
verKeyH6Depr = "cc_cold1lmaet9hdvu9d9jvh34u0un4ndw3yewaq5ch6fnwsctw02xxwylj"

-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#cip-0129-compliant-constitutional-committee-cold-verification-key-hash-appended-with--12-hex-encoded-byte-constitutional-committee-cold-key-hash-credential
-- [CIP-0129 compliant] Constitutional Committee Cold Verification key hash appended with '12' hex-encoded byte (Constitutional Committee Cold key hash credential)
verKeyH6Cred :: String
verKeyH6Cred = "cc_cold1ztl0h9vka4ns45kfj7xh3ljwkd46yn9m5znzlfxd6rpdeagw6p59q"

-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#constitutional-committee-cold-verification-key
-- Constitutional Committee Cold Verification Key
verKey6 :: String
verKey6 = "cc_cold_vk149up407pvp9p36lldlp4qckqqzn6vm7u5yerwy8d8rqalse3t04q7qsvwl"

-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0105/test-vectors/test-vector-1.md#constitutional-committee-cold-extended-verification-key
-- Constitutional Committee Cold Extended Verification Key
xVerKey6 :: String
xVerKey6 = "cc_cold_xvk149up407pvp9p36lldlp4qckqqzn6vm7u5yerwy8d8rqalse3t04vvqvk3e6l7vzjl7n8ttk646jflumvkgcrdhcstc5wr5etg5n7dnc8nqv5d"

verKeyH7 :: String
verKeyH7 = "cc_hot1qg6ck5scr3y0wjzf9jjxjl7wcslpy4yfzjesxyw5qg4knyg9ckh0d"

verKey7 :: String
verKey7 = "cc_hot_vk1a5q4r34xzm0r6y728d4gmrl7jvrfuh7r022k7wh5mzwmyg7d7l3s3fzqkv"

xVerKey7 :: String
xVerKey7 = "cc_hot_xvk1a5q4r34xzm0r6y728d4gmrl7jvrfuh7r022k7wh5mzwmyg7d7l3hjwwaw54qwj0rn084enysj8ha2vwg2wd7umksf4tcnskaj8xr4tcempwly"
