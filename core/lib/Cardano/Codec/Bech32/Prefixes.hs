{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{- HLINT ignore "Use camelCase" -}

-- | List common bech32 prefixes used for objects in the Cardano eco-systems.
--
-- As specified in [CIP-5](https://github.com/cardano-foundation/CIPs/tree/master/CIP5)
module Cardano.Codec.Bech32.Prefixes where

import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )


-- Addresses

addr :: HumanReadablePart
addr = [humanReadablePart|addr|]

addr_test :: HumanReadablePart
addr_test = [humanReadablePart|addr_test|]

script :: HumanReadablePart
script = [humanReadablePart|script|]

stake :: HumanReadablePart
stake = [humanReadablePart|stake|]

stake_test :: HumanReadablePart
stake_test = [humanReadablePart|stake_test|]


-- Keys

addr_vk :: HumanReadablePart
addr_vk = [humanReadablePart|addr_vk|]

addr_sk :: HumanReadablePart
addr_sk = [humanReadablePart|addr_sk|]

addr_xvk :: HumanReadablePart
addr_xvk = [humanReadablePart|addr_xvk|]

addr_xsk :: HumanReadablePart
addr_xsk = [humanReadablePart|addr_xsk|]

acct_vk :: HumanReadablePart
acct_vk = [humanReadablePart|acct_vk|]

acct_sk :: HumanReadablePart
acct_sk = [humanReadablePart|acct_sk|]

acct_xvk :: HumanReadablePart
acct_xvk = [humanReadablePart|acct_xvk|]

acct_xsk :: HumanReadablePart
acct_xsk = [humanReadablePart|acct_xsk|]

root_vk :: HumanReadablePart
root_vk = [humanReadablePart|root_vk|]

root_sk :: HumanReadablePart
root_sk = [humanReadablePart|root_sk|]

root_xvk :: HumanReadablePart
root_xvk = [humanReadablePart|root_xvk|]

root_xsk :: HumanReadablePart
root_xsk = [humanReadablePart|root_xsk|]

script_vk :: HumanReadablePart
script_vk = [humanReadablePart|script_vk|]

script_sk :: HumanReadablePart
script_sk = [humanReadablePart|script_sk|]

script_xvk :: HumanReadablePart
script_xvk = [humanReadablePart|script_xvk|]

script_xsk :: HumanReadablePart
script_xsk = [humanReadablePart|script_xsk|]

stake_vk :: HumanReadablePart
stake_vk = [humanReadablePart|stake_vk|]

stake_sk :: HumanReadablePart
stake_sk = [humanReadablePart|stake_sk|]

stake_xvk :: HumanReadablePart
stake_xvk = [humanReadablePart|stake_xvk|]

stake_xsk :: HumanReadablePart
stake_xsk = [humanReadablePart|stake_xsk|]


-- Hashes

addr_vkh :: HumanReadablePart
addr_vkh = [humanReadablePart|addr_vkh|]

script_vkh :: HumanReadablePart
script_vkh = [humanReadablePart|script_vkh|]

stake_vkh :: HumanReadablePart
stake_vkh = [humanReadablePart|stake_vkh|]
