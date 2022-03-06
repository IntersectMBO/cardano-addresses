{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{- HLINT ignore "Use camelCase" -}

-- | List common bech32 prefixes used for objects in the Cardano eco-systems.
--
-- As specified in [CIP-5](https://github.com/cardano-foundation/CIPs/tree/master/CIP5)
module Cardano.Codec.Bech32.Prefixes
    ( -- * Addresses
      addr
    , addr_test
    , script
    , stake
    , stake_test

      -- * Hashes
    , addr_vkh
    , stake_vkh
    , addr_shared_vkh
    , stake_shared_vkh

      -- * Keys for 1852H
    , addr_vk
    , addr_sk
    , addr_xvk
    , addr_xsk
    , acct_vk
    , acct_sk
    , acct_xvk
    , acct_xsk
    , root_vk
    , root_sk
    , root_xvk
    , root_xsk
    , stake_vk
    , stake_sk
    , stake_xvk
    , stake_xsk

      -- * Keys for 1854H
    , addr_shared_vk
    , addr_shared_sk
    , addr_shared_xvk
    , addr_shared_xsk
    , acct_shared_vk
    , acct_shared_sk
    , acct_shared_xvk
    , acct_shared_xsk
    , root_shared_vk
    , root_shared_sk
    , root_shared_xvk
    , root_shared_xsk
    , stake_shared_vk
    , stake_shared_sk
    , stake_shared_xvk
    , stake_shared_xsk

      -- * Keys for 1855H
    , policy_vk
    , policy_xvk
    , policy_vkh
    , policy_xsk
    ) where

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

stake_vk :: HumanReadablePart
stake_vk = [humanReadablePart|stake_vk|]

stake_sk :: HumanReadablePart
stake_sk = [humanReadablePart|stake_sk|]

stake_xvk :: HumanReadablePart
stake_xvk = [humanReadablePart|stake_xvk|]

stake_xsk :: HumanReadablePart
stake_xsk = [humanReadablePart|stake_xsk|]

addr_shared_vk :: HumanReadablePart
addr_shared_vk = [humanReadablePart|addr_shared_vk|]

addr_shared_sk :: HumanReadablePart
addr_shared_sk = [humanReadablePart|addr_shared_sk|]

addr_shared_xvk :: HumanReadablePart
addr_shared_xvk = [humanReadablePart|addr_shared_xvk|]

addr_shared_xsk :: HumanReadablePart
addr_shared_xsk = [humanReadablePart|addr_shared_xsk|]

acct_shared_vk :: HumanReadablePart
acct_shared_vk = [humanReadablePart|acct_shared_vk|]

acct_shared_sk :: HumanReadablePart
acct_shared_sk = [humanReadablePart|acct_shared_sk|]

acct_shared_xvk :: HumanReadablePart
acct_shared_xvk = [humanReadablePart|acct_shared_xvk|]

acct_shared_xsk :: HumanReadablePart
acct_shared_xsk = [humanReadablePart|acct_shared_xsk|]

root_shared_vk :: HumanReadablePart
root_shared_vk = [humanReadablePart|root_shared_vk|]

root_shared_sk :: HumanReadablePart
root_shared_sk = [humanReadablePart|root_shared_sk|]

root_shared_xvk :: HumanReadablePart
root_shared_xvk = [humanReadablePart|root_shared_xvk|]

root_shared_xsk :: HumanReadablePart
root_shared_xsk = [humanReadablePart|root_shared_xsk|]

stake_shared_vk :: HumanReadablePart
stake_shared_vk = [humanReadablePart|stake_shared_vk|]

stake_shared_sk :: HumanReadablePart
stake_shared_sk = [humanReadablePart|stake_shared_sk|]

stake_shared_xvk :: HumanReadablePart
stake_shared_xvk = [humanReadablePart|stake_shared_xvk|]

stake_shared_xsk :: HumanReadablePart
stake_shared_xsk = [humanReadablePart|stake_shared_xsk|]

-- Hashes

addr_vkh :: HumanReadablePart
addr_vkh = [humanReadablePart|addr_vkh|]

stake_vkh :: HumanReadablePart
stake_vkh = [humanReadablePart|stake_vkh|]

addr_shared_vkh :: HumanReadablePart
addr_shared_vkh = [humanReadablePart|addr_shared_vkh|]

stake_shared_vkh :: HumanReadablePart
stake_shared_vkh = [humanReadablePart|stake_shared_vkh|]

-- Policy
policy_vk :: HumanReadablePart
policy_vk = [humanReadablePart|policy_vk|]

policy_xvk :: HumanReadablePart
policy_xvk = [humanReadablePart|policy_xvk|]

policy_vkh :: HumanReadablePart
policy_vkh = [humanReadablePart|policy_vkh|]

policy_xsk :: HumanReadablePart
policy_xsk = [humanReadablePart|policy_xsk|]
