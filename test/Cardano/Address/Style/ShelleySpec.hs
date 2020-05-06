{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Address.Style.ShelleySpec
    ( spec
    ) where

import Prelude

import Cardano.Address
    ( bech32WithHrp, hex, unsafeMkAddress )
import Cardano.Address.Derivation
    ( AccountingStyle (..)
    , Depth (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index
    , SoftDerivation (..)
    , XPrv
    , toXPub
    , xprvToBytes
    , xpubToBytes
    )
import Cardano.Address.Style.Shelley
    ( Shelley (..) )
import Cardano.Mnemonic
    ( SomeMnemonic, mkSomeMnemonic )
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Data.Text
    ( Text )
import Test.Arbitrary
    ()
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , expectFailure
    , property
    , vector
    , (===)
    , (==>)
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_publicChildKeyDerivation

    describe "Bounded / Enum relationship" $ do
        it "Calling toEnum for invalid value gives a runtime err (AccountingStyle)"
            (property prop_toEnumAccountingStyle)

    describe "Enum Roundtrip" $ do
        it "AccountingStyle" (property prop_roundtripEnumAccountingStyle)

    describe "Golden tests" $ do
        goldenTest TestVector
            { rootXPrv = "root_xprv982a5cfec6be5656be247f0060d13d3bd52403bcca62\
                         \7f1d9988ca665907c5400980a71f60e32c6efa5de59bbedd4add1\
                         \8f57955f7fe0ca434f8b6ddb48ec72202545a3943a396ccdcb0fb\
                         \98fffd989635fd93406b04f556ea54034012b72008"
            , accXPrv = "acct_xprv1xqwvl9u488wkkywa9pfr8vncxf6sk6reea878ytt8pgr\
                        \ses8c4q9wl66mqw6gtcz7jrw2x5nh0c9vxl4u4yk35qmuqs62hwmfw\
                        \8g59nqqzsw5cdy26sfmu8y8q9g36w0ukrh62g5x9x2r3p7g6x8z064\
                        \nulavjdy"
            , addrXPrv0 = "addr_xprv1dzrs9tvy632mtzvjgrvrkdr0j96j4dwaej06lytjah\
                          \vzquq8c4qzqws3zp2uc8rmrmhzrhukjml9vrfqxwhpjs78yfh6ny\
                          \v9cpk9n87e4rsmperder5gf9wje76zuq39w7vaefaehkvd0vyt08\
                          \02sryq053jdle4"
            , addrXPub0 = "addr_xpub1lrh9ds9kxhhnhqmpqfsj3wrf48mcs0m8r95c9ggdwe\
                          \3cltc7xvran28pkrjxmj8gsj2a9na59cpz2auemjnmn0vc67cgk7\
                          \w74qxgqlg0ydscd"
            , addrXPrv1 = "addr_xprv16qyf2nkx9t7qunjqza480djyfednzjrsa0407qyuje\
                          \xnzag8c4qz48ctgnzfgw8m8mdawu9wg2d7ezt27zfexp6ffzpz36\
                          \spycusdteq898ucwnylsl36xhdjum7xa8mtw4ga358qkv80dppmw\
                          \yjmgy7g5ke0uyw"
            , addrXPub1 = "addr_xpub10thqpnm6uuksmr04tdwz6vlgm6wrnccd5mlglzjkr3\
                          \9l6e2pqa4jqw20esaxflplr5dwm9ehud60kka23mrgwpvcw76zrk\
                          \uf9ksfu3gvlxlc9"
            , addrXPrv1442 = "addr_xprv1pqtsmncr23nfc86v4pallgj6yv2pfttalmjpa2u\
                             \nme08gug8c4qgz9jme868mkxrjen2hfwcu6swa6s7z608gy2y\
                             \62l2082698fkvvkvld3hnqave3c98llkdelz0qa9frxyjxggf\
                             \azye0g2ptp5k7vpgcuqa764"
            , addrXPub1442 = "addr_xpub1xq255jrlf0cxwu9kqe9sdyljhvn972dez5f2fg2\
                             \zfpc4dt0fgk5ue7mr0xp6enrs20llvmn7y7p62jxvfyvssn6y\
                             \fj7s5zkrfducz3sc6gjjx"
            , mnemonic = [ "test", "child", "burst", "immense", "armed", "parrot"
                         , "company", "walk", "dog" ]
            }
        goldenTest TestVector
            { rootXPrv = "root_xprv608621fb4c0101feb31f6f2fd7018bee54101ff67d55\
                         \5079671893225ee1a45e2331497029d885b5634405f350508cd95\
                         \dce3991503b10f128d04f34b7b625783a1e3bd5dcf11fd4f989ec\
                         \2cdcdea3a54db8997398174ecdcc87006c274176a0"
            , accXPrv = "acct_xprv12phl7y4uv58mne08me5szrwly2gn6jasqmkvcrndq762\
                        \q68p5302z24gdf365klhll2a5f357k7nc4kpaq7j6agr5m22jq4jwl\
                        \fv6l505h7dg64an4rdfk9f028nge0zcn508jw6m8lkdq36zc0v4h9x\
                        \qs762yl0"
            , addrXPrv0 = "addr_xprv1hqf6v2lvhfn5mr3fe6g8ac6n8a3z6s0p24mg6kre8j\
                          \adxulp530y07wjp2ml0zcz8gk0xc7zy96qp2xxtr0arjq9038k9d\
                          \hkw3k3cswawhs4fkjp00kwc4wd6fynyaz5zw8ssggs9974apatyh\
                          \s4ltg4puskm3kd"
            , addrXPub0 = "addr_xpub1w0l2sr2zgfm26ztc6nl9xy8ghsk5sh6ldwemlpmp9x\
                          \ylzy4dtf7a6a0p2ndyz7lva32um5jfxf69gyu0pqs3q2tat6r6kf\
                          \0pt7k32rcmm5vlw"
            , addrXPrv1 = "addr_xprv1up30yhwmkujfkjd2rvy99z7qg7mdpqd8n7vehsyvfq\
                          \20zulp530weme6mjk3yuqkynms9tr23wqczk94npvnp4r8l2vrq3\
                          \6l8vudk8lh4vfx72yymwg9n7sfe2pmu6ut6qjsgf4wkcser0wesc\
                          \290w9ujyg4af0a"
            , addrXPub1 = "addr_xpub17cn2hzr7kh6qk5pyv0x09mz6wvgkwmhfuh24cjfqtx\
                          \3kds956jsl02cjdu5gfkust8aqnj5rhe4ch5p9qsn2ad3pjx7anp\
                          \s527uteyg7w980n"
            , addrXPrv1442 = "addr_xprv14qtc6t6wfzk4jvdp0k8d9jp0xtlmhkh8el98nfn\
                             \qwr5kqulp530vmhqefsml4rsntysxu057jecdq0l3gmzs6c8n\
                             \v5mvjn5aesmklzqepr3e4n4ca0p23tm57dwkw4ws46cz2mmdx\
                             \3jnxtxy8zqyfekftvu7wdvm"
            , addrXPub1442 = "addr_xpub1l7vftt4ayg29thue83m2crav65wqzak2v9v7lry\
                             \jnnphq88rsrr3jz8rnt8t367z4zhhfu6ava2apt4sy4hk6dr9\
                             \xvkvgwyqgnnvjkcrvvkp8"
            , mnemonic = [ "test", "walk", "nut", "penalty", "hip", "pave", "soap",
                           "entry", "language", "right", "filter", "choice" ]
            }
        goldenTest TestVector
            { rootXPrv = "root_xprvb8f2bece9bdfe2b0282f5bad705562ac996efb6af96b\
                         \648f4445ec44f47ad95c10e3d72f26ed075422a36ed8585c745a0\
                         \e1150bcceba2357d058636991f38a3791e248de509c070d812ab2\
                         \fda57860ac876bc489192c1ef4ce253c197ee219a4"
            , accXPrv = "acct_xprv1crpfhmtv5vdjx9rsqre7z025gartudsanenhart5nh3m\
                        \uqnmm9wvu7jr0c2neykydr3r85rw6vht8sg0u4dffts2klnadgk9rt\
                        \8zh23kfqwxrlk3feq0dvvfvd8htlpnhmpu2fpklpl09th296pm7sdc\
                        \6qafmpwv"
            , addrXPrv0 = "addr_xprv1fzgcl9km0mve2jwe8qxve364w6te9vpddhwpw5g8wn\
                          \jlupmmm9wxpdda6jaglx7smwl6qd5xuzjcweeq8ykp0wg9hng4pg\
                          \6eumwx2t90swaed7ehsa6j86qsw3fnl4thtemsng6vukmz6ddf3c\
                          \nd4sfkzuqvwsgp"
            , addrXPub0 = "addr_xpub1fz009r4f0aceaemksezlca9cz8p8rewhaurvyvgg2n\
                          \dnq9vwj3w6lqamjman0pm4y05pqazn8l2hwhnhpx35eedk9566nr\
                          \3xmtqnv9clhnw7d"
            , addrXPrv1 = "addr_xprv1lq2ylz7fhsn0dfmul2pe833cdwvjnvux9uaxuzaz50\
                          \gs7pnmm9wq343uh5cpfs87tgh9saa86un8e2l266rsge0c5qsmta\
                          \ud5r64ctndwkyth8q07fgusyr3fldhn6lgd5tat5cmcdzvfzhtd0\
                          \cpsleuxg0f45l9"
            , addrXPub1 = "addr_xpub1y3r70ejyadsaplez83p7uhy8p6l08a5sjl860kszev\
                          \xu0jaxcwmx6avghwwqluj3eqg8zn7m0847smgh6hf3hs6ycj9wk6\
                          \lsrplncvspv5k5u"
            , addrXPrv1442 = "addr_xprv14r6s3v6xu2j33mjlq2ugxf9345sz658rxs9csld\
                             \ztegazrrmm9wdsyv8k3df6q2t32ngnvlgvyw8vjy6g5540ap3\
                             \sxajcrm04ruq9wvgxqkej8z8xkt06guqj7wtafqgrstdeukd5\
                             \pnn8azvhpm2taczzgpnkuuu"
            , addrXPub1442 = "addr_xpub1fqtu4wksrftpdxg5ayacthraykm8eflvnym7lgc\
                             \4xlzz479dve5csvpdnywywdvkl53cp9uuh6jqs8qkmnevmgr8\
                             \x06yewrk5hmsyysvsfywe"
            , mnemonic = [ "art", "forum", "devote", "street", "sure", "rather",
                           "head", "chuckle", "guard", "poverty", "release",
                           "quote", "oak", "craft", "enemy"]
            }
        goldenTest TestVector
            { rootXPrv = "root_xprve8aa0904f407272f5367b0dac6911b66b95c64123b26\
                         \eea627725ba29316875e972fd37e121a09dbbacbc7c8ad118d34c\
                         \212ddaf564e2d7198d6c3f9d6d4cb6ce0387dd6b5c0393c08c239\
                         \cd9b1eaf3578d23d0c32edb0bc4d41a57011c6d2d9"
            , accXPrv = "acct_xprv1vzjuw65sf27s478y8ndz2jpan4rev52h0fdh9z3tdwcp\
                        \lgqksa0wa9wntpw4d74rhj8rs2unf6rfujxu90mstt8ztnjrn80r8s\
                        \z0edzf22jznx2n63qdalqm2tphlfn7k6qxtm0pg85z4ptk639vyjju\
                        \fccch84r"
            , addrXPrv0 = "addr_xprv17zj2lhjk379klp40xfzsad0yzygqe45uaggnkzf4ld\
                          \3emgsksa00ftq88fnfjxg245kjjqcukyjfg4lwmf3r2qqymyyqen\
                          \nch3y8llyg0d629pdx0pp0l69lerjz75kxmk5e6cr2d82kafp7a2\
                          \5y0qy5fvvkjv84"
            , addrXPub0 = "addr_xpub1fwgdh5vv6akdc3rjpeq57xxq4lc9m84xcrt6q827mq\
                          \7u20wuw54gs7m552z6v7zzll5tlj8y9afvdhdfn4sx56w4d6jra6\
                          \4gg7qfgjcq4pu8d"
            , addrXPrv1 = "addr_xprv1wz99hznmt96crxthcmnxqttaul6caq4hv5jwttd5ll\
                          \y2mfsksa08q68skn2ggclu6vf40phx3wnj4e8fvxed6at8xxekwa\
                          \49rg4c3ec8kp2nwcxfw6sgxphzckg5v0dausldvya0w6jy5k3cxw\
                          \rqdjsthqpfkqw6"
            , addrXPub1 = "addr_xpub135hqmkaqydnxnq6wmjkkhasvwjprpnqnzsrwwes6mq\
                          \l45enlcsqs0vz4xasvja4qsvrw93v3gc7mmep76cf67a4yffdrsv\
                          \uxqm9qhwqcq4379"
            , addrXPrv1442 = "addr_xprv15pjw7rg5ywgfe4js8dufp6ul4kccpn9gdxtw0hn\
                             \t4nk2h2gksa09e6p79my6p4k4wggyt482s88zzdhg0fkjgv3t\
                             \s63z32k9rxc022t0054563zytwgcd6x5g8zvsrxkgyldr559y\
                             \dtz98s3p5tavtdy6ywzxmsd"
            , addrXPub1442 = "addr_xpub1kvsa8pjxlg93nk87tdp08tauy2uc4hdy36xufkj\
                             \lgdz2dm7p6pxx7lftf4zygku3sm5dgswyeqxdvsf768fg2g6k\
                             \y20pzrgh6ck6f5gyh7uf4"
            , mnemonic = [ "churn", "shaft", "spoon", "second", "erode", "useless",
                           "thrive", "burst", "group", "seed", "element", "sign",
                           "scrub", "buffalo", "jelly", "grace", "neck", "useless" ]
            }
        goldenTest TestVector
            { rootXPrv = "root_xprvf080e8d14493d50c91612f43c9e714a794ab16381ac3\
                         \f01f1740537f60d18b515628d17049769717bdad315c2312f9b19\
                         \20bb053fa4c97267807a26e4fb4389785f9411155f742692668d3\
                         \3f263a77108908749911a3286941d5b3989a093666"
            , accXPrv = "acct_xprv13rkczfa4xzrax4x0unqnesaasr8s3qnyr2mmjcwga6lr\
                        \ue733dg6at2stlqft9jtz0kk50xsp4yrqnrf9gl78npu50r7e38kpu\
                        \0f744af46s2r37d7m6sh0uyj9szp86s546k43t2thfw4nzd7zsxckx\
                        \6ggwkjgx"
            , addrXPrv0 = "addr_xprv1fz8tz0pdda8la0aqhadnzctw0p48zwygkgf4xyar2j\
                          \jljm733dgkprs4sj8cxfwv9xtfddpdfvjlap0hhg9gd37pr0tp7u\
                          \e48mh9cnfyy68k52f88z5vghezam30c3pcue6aewl4mqul6nvass\
                          \xlenh3eqf7zg7r"
            , addrXPub0 = "addr_xpub1x4dme9s2f5xxn77wgjhggqh73r6syy4nvjcdjklnaq\
                          \rh48f6desjgf50dg5jww9gc30j9mhzl3zr3en4mjaltkpel4xemp\
                          \qdln80rjqny9hz0"
            , addrXPrv1 = "addr_xprv18peu0v64maghaa87jvu0txdkftvznq7he2yhntk8ee\
                          \m56mk33dgl2rwt8kmhcgdytr6fjn0t4cdf6sr3xud67yhwnjhzyg\
                          \hgu294f6v0fcfzqlactzd8cf5m4tpu7yyn5x58dx6q00d362j6e0\
                          \6g88phjgp2cnfa"
            , addrXPub1 = "addr_xpub1ndtepmpg06x9nskfasvr50mue356e4rqlvuzf8jjcj\
                          \6n48feexsg7nsjyplmsky60snfh2kreugf8gdgw6d5q77mr5494j\
                          \l5swwr0ysx0msxw"
            , addrXPrv1442 = "addr_xprv1sr8gz532k2yckskgzv4047taz92905nnr9sysv2\
                             \0ks257ux33dg5000a39wpvw066n3pcgz20062yhqpy9mjjszl\
                             \uq5ef9mg5yjjsseur0tu9duqet2zxeqfm9jlt6g4wf3altrup\
                             \qwggrn00j0f36v8aq8x9v3f"
            , addrXPub1442 = "addr_xpub1vagd70p45qd774cdna2qe2t3t9200hx3jcfvp39\
                             \w3r9ruscjapdncx7hc2mcpjk5ydjqnkt97h532unrm7k8czqu\
                             \ss8x7ly7nr5c06qd3aex3"
            , mnemonic = [ "draft", "ability", "female", "child", "jump", "maid",
                           "roof", "hurt", "below", "live", "topple", "paper",
                           "exclude", "ordinary", "coach", "churn", "sunset",
                           "emerge", "blame", "ketchup", "much" ]
            }
        goldenTest TestVector
            { rootXPrv = "root_xprv989c07d00e8d3d187c1855f25468e013fb1bcad93430\
                         \fc5a298a259802118f451e846b73a627c54ab13d49f5995b8563b\
                         \32ad860c019a28b0b953209cd11bc1843548a19d62a34d1714b1a\
                         \c21903524efffaba00c4f4fcb203649661b61e2ca6"
            , accXPrv = "acct_xprv1grmww8c9yftkd6nlmuh7wypx46duh8az7sxyg88fr4k3\
                        \qpc33azk8gc5ntllturxzee5gj2zd5dy48f0ehp6lqudkwvacxjznz\
                        \8j0mzd2ad0t2fmmaystgms97k7maz3afvy0ywjxwk7jzt96cyt43tn\
                        \qsxye0df"
            , addrXPrv0 = "addr_xprv1zq578drze4tp8fq43gur62v2j57vd5nvg862jc8hz5\
                          \f959g33azuy3hl7ce8cjrxua5nyarqr9f4aatqecjsq9gturmqrc\
                          \pkwuhu5rchassddgsr6uaa5vzk80fq72wg8fejl9fyc94f8vvp0x\
                          \904augp5nuw49r"
            , addrXPub0 = "addr_xpub1lytzhygjvgftw9gqazwu0k33zywlc9rx48ey7j9rfe\
                          \l222wj6vup0mpq663q84emmgc9vw7jpu5uswnn972jfst2jwccz7\
                          \v2ltmcsrgulk37x"
            , addrXPrv1 = "addr_xprv1pq9m55k5gwl6kghjmdxxhlvc24nah84e3v7mrcj5d8\
                          \2vjyq33azlqg7n29xrmhumpmnrnqkufxfqasp2f5d9g4ptg060fa\
                          \5k8csvshmm95jmjensafguy23ce0l2rq3cx54u3csmxc6pd7eclu\
                          \4a5jyqjsnp2xvx"
            , addrXPub1 = "addr_xpub1vh7dfgcpejhgsvf3h9splavunc2lktzq6pywvhd77a\
                          \mer2etywa8ktf9h9n8p6j3cg4r3jl75xprsdfter3pkd35zman3l\
                          \etmfygp9qxjcsku"
            , addrXPrv1442 = "addr_xprv18zk3n3ue6gzwqn6gqyxe4cly6mgfy62q4x9qdz3\
                             \tketqxrc33azulyvcataalffml635vynej9esk88f6qzvak5d\
                             \x7tk0fh54e5z95st8s7m3mkxfzgujxf4s00k3k9qn3e6rfd34\
                             \xy4anvxd742e4z30qq6ckqk"
            , addrXPub1442 = "addr_xpub1qlsdp05jtlsyqrxft7cmyjuww0f85ppaknzkszh\
                             \8q3dh9cwqspmqk0pahrhvvjy3eyvntq7ldrv2p8rn5xjmr2vf\
                             \tmxcvma24n29z7qk22fyr"
            , mnemonic = [ "excess", "behave", "track", "soul", "table", "wear",
                           "ocean", "cash", "stay", "nature", "item", "turtle",
                           "palm", "soccer", "lunch", "horror", "start", "stumble",
                           "month", "panic", "right", "must", "lock", "dress" ]
            }

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

prop_publicChildKeyDerivation
    :: (SomeMnemonic, SndFactor)
    -> AccountingStyle
    -> Index 'Soft 'AddressK
    -> Property
prop_publicChildKeyDerivation (mw, (SndFactor sndFactor)) cc ix =
    addrXPub1 === addrXPub2
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv minBound
    addrXPub1 = toXPub <$> deriveAddressPrivateKey accXPrv cc ix
    addrXPub2 = deriveAddressPublicKey (toXPub <$> accXPrv) cc ix

prop_accountKeyDerivation
    :: (SomeMnemonic, SndFactor)
    -> Index 'Hardened 'AccountK
    -> Property
prop_accountKeyDerivation (mw, (SndFactor sndFactor)) ix =
    accXPrv `seq` property ()
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv = deriveAccountPrivateKey rootXPrv ix

prop_toEnumAccountingStyle :: Int -> Property
prop_toEnumAccountingStyle n =
    n > fromEnum UTxOInternal ==> expectFailure $ property $
        (toEnum n :: AccountingStyle) `seq` ()

prop_roundtripEnumAccountingStyle :: AccountingStyle -> Property
prop_roundtripEnumAccountingStyle ix =
    (toEnum . fromEnum) ix === ix

{-------------------------------------------------------------------------------
                             Golden tests
-------------------------------------------------------------------------------}

data TestVector = TestVector
    {
      -- | Definitions: Let's assume we have private key  < xprv | pub | cc >
      -- Then, extended private key is understood as < xprv |     | cc >
      --       extended public key  is understood as <      | pub | cc >

      -- | The extended root private key hex-encoded, prefixed with 'root_xprv'
      rootXPrv :: Text

      -- | The extended 0th account private key, bech32 encoded prefixed with 'acct_xprv'
    , accXPrv :: Text

      -- | The extended 0th address private key, bech32 encoded prefixed with 'addr_xprv'
    , addrXPrv0 :: Text

      -- | The extended 0th address public key, bech32 encoded prefixed with 'addr_xpub'
    , addrXPub0 :: Text

      -- | The extended 1st address private key, bech32 encoded prefixed with 'addr_xprv'
    , addrXPrv1 :: Text

      -- | The extended 1st address public key, bech32 encoded prefixed with 'addr_xpub'
    , addrXPub1 :: Text

      -- | The extended 1442nd address private key, bech32 encoded prefixed with 'addr_xprv'
    , addrXPrv1442 :: Text

      -- | The extended 1442nd address public key, bech32 encoded prefixed with 'addr_xpub'
    , addrXPub1442 :: Text

      -- | Corresponding Mnemonic
    , mnemonic :: [Text]
    }

goldenTest :: TestVector -> SpecWith ()
goldenTest TestVector{..} = it (show $ T.unpack <$> mnemonic) $ do
    let (Right mw) = mkSomeMnemonic @'[9,12,15,18,21,24] mnemonic
    let sndFactor = mempty
    let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    let rootXPrv' =
            T.append "root_xprv" (T.decodeUtf8 $ hex $ xprvToBytes $ getKey rootK)
    rootXPrv' `shouldBe` rootXPrv

    let accIx = toEnum 0x80000000
    let acctK = deriveAccountPrivateKey rootK accIx
    let (Right hrp) = Bech32.humanReadablePartFromText "acct_xprv"
    let accXPrv' = bech32WithHrp hrp $ getExtendedKeyAddr acctK
    accXPrv' `shouldBe` accXPrv

    let (Right hrpPrv) = Bech32.humanReadablePartFromText "addr_xprv"
    let (Right hrpPub) = Bech32.humanReadablePartFromText "addr_xpub"
    let addIx0 = toEnum 0x00000000
    let addrK0prv = deriveAddressPrivateKey acctK UTxOExternal addIx0
    let addrXPrv0' = bech32WithHrp hrpPrv $ getExtendedKeyAddr addrK0prv
    addrXPrv0' `shouldBe` addrXPrv0
    let addrXPub0' = bech32WithHrp hrpPub $ getPublicKeyAddr $ toXPub <$> addrK0prv
    addrXPub0' `shouldBe` addrXPub0
    let addIx1 = toEnum 0x00000001
    let addrK1prv = deriveAddressPrivateKey acctK UTxOExternal addIx1
    let addrXPrv1' = bech32WithHrp hrpPrv $ getExtendedKeyAddr addrK1prv
    addrXPrv1' `shouldBe` addrXPrv1
    let addrXPub1' = bech32WithHrp hrpPub $ getPublicKeyAddr $ toXPub <$> addrK1prv
    addrXPub1' `shouldBe` addrXPub1
    let addIx1442 = toEnum 0x000005a2
    let addrK1442prv = deriveAddressPrivateKey acctK UTxOExternal addIx1442
    let addrXPrv1442' = bech32WithHrp hrpPrv $ getExtendedKeyAddr addrK1442prv
    addrXPrv1442' `shouldBe` addrXPrv1442
    let addrXPub1442' = bech32WithHrp hrpPub $ getPublicKeyAddr $ toXPub <$> addrK1442prv
    addrXPub1442' `shouldBe` addrXPub1442

  where
    getExtendedKeyAddr = unsafeMkAddress . xprvToBytes . getKey
    getPublicKeyAddr = unsafeMkAddress . xpubToBytes . getKey

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

newtype SndFactor = SndFactor ScrubbedBytes
    deriving stock (Eq, Show)
    deriving newtype (ByteArrayAccess)

instance Arbitrary SndFactor where
    arbitrary = do
        n <- choose (0, 64)
        bytes <- BS.pack <$> vector n
        return $ SndFactor $ BA.convert bytes
