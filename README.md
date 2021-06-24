<p align="center">
  <big><strong>Cardano Addresses</strong></big>
</p>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-addresses/releases"><img src="https://img.shields.io/github/v/release/input-output-hk/cardano-addresses?color=%239b59b6&label=RELEASE&sort=semver&style=for-the-badge"/></a>
  <a href='https://github.com/input-output-hk/cardano-addresses/actions?query=workflow%3A"Continuous Integration (Linux)"'><img src="https://img.shields.io/github/workflow/status/input-output-hk/cardano-addresses/Continuous%20Integration%20(Linux)?style=for-the-badge&label=BUILD%20(Linux)" /></a>
  <a href='https://github.com/input-output-hk/cardano-addresses/actions?query=workflow%3A"Continuous Integration (Windows)"'><img src="https://img.shields.io/github/workflow/status/input-output-hk/cardano-addresses/Continuous%20Integration%20(Windows)?style=for-the-badge&label=BUILD%20(Windows)" /></a>
  <a href='https://github.com/input-output-hk/cardano-addresses/actions?query=workflow%3A"TypeScript NPM Package"'><img src="https://img.shields.io/github/workflow/status/input-output-hk/cardano-addresses/TypeScript%20NPM%20Package?style=for-the-badge&label=BUILD%20(TypeScript)" /></a>
  <a href="https://input-output-hk.github.io/cardano-addresses/coverage/hpc_index.html"><img src="https://input-output-hk.github.io/cardano-addresses/coverage/badge.svg" /></a>
</p>

## Overview

This module provides mnemonic (backup phrase) creation, and conversion of a
mnemonic to seed for wallet restoration, and address derivation functionalities.

![](.github/example.gif)

## Documentation

API documentation is available [here](https://input-output-hk.github.io/cardano-addresses/haddock).

## Command-Line

`cardano-address` comes with a command-line interface for Linux. See the [release artifacts](https://github.com/input-output-hk/cardano-addresses/releases) or [continuous integration artifacts](https://github.com/input-output-hk/cardano-addresses/actions?query=workflow%3A%22Continuous+Integration%22) to get a pre-compiled binary, or [build a Docker image](#docker-image). The command-line is self explanatory by using `--help` on various commands and sub-commands.

> :bulb: Most commands read argument from the standard input. This prevent sensitive information from appearing into your shell history and, makes it easy to pipe commands!

<details>
  <summary>How to generate a recovery phrase (<strong>phrase.prv</strong>)</summary>

```console
$ cardano-address recovery-phrase generate --size 15 > phrase.prv
exercise club noble adult miracle awkward problem olympic puppy private goddess piano fatal fashion vacuum
```
</details>

<details>
  <summary>How to generate a root private key (<strong>root.xsk</strong>)</summary>

```console
$ cardano-address key from-recovery-phrase Shelley < phrase.prv > root.xsk
root_xsk1hqzfzrgskgnpwskxxrv5khs7ess82ecy8za9l5ef7e0afd2849p3zryje8chk39nxtva0sww5me3pzkej4rvd5cae3q3v8eu7556n6pdrp4fdu8nsglynpmcppxxvfdyzdz5gfq3fefjepxhvqspmuyvmvqg8983
```

> :information_source: Notice the `root_xsk` prefix to identify a root extended signing (private) key.
</details>

<details>
  <summary>How to generate a payment verification key (<strong>addr.xvk</strong>)</summary>

```console
$ cardano-address key child 1852H/1815H/0H/0/0 < root.xsk | cardano-address key public --with-chain-code > addr.xvk
addr_xvk1grvg8qzmkmw2n0dm4pd0h3j4dv6yglyammyp733eyj629dc3z28v6wk22nfmru6xz0vl2s3y5xndyd57fu70hrt84c6zkvlwx6fdl7ct9j7yc
```

> :information_source: The last segment in the path is the key index and can be incremented up to `2^31-1` to derive more keys.
</details>

<details>
  <summary>How to generate a stake verification key (<strong>stake.xvk</strong>)</summary>

```console
$ cardano-address key child 1852H/1815H/0H/2/0 < root.xsk | cardano-address key public --with-chain-code > stake.xvk
stake_xvk1658atzttunamzn80204khrg0qfdk5nvmrutlmmpg7xlsyaggwa7h9z4smmeqsvs67qhyqmc2lqa0vy36rf2la74ym8a5p93zp4qtpuq6ky3ve
```

> :information_source: The last segment in the path is the key index and can be incremented up to `2^31-1` to derive more keys.
</details>

<details>
  <summary>How to generate a payment address from a payment key (<strong>payment.addr</strong>)</summary>

```console
$ cardano-address address payment --network-tag testnet < addr.xvk > payment.addr
addr_test1vp2fg770ddmqxxduasjsas39l5wwvwa04nj8ud95fde7f7guscp6v
```
</details>

<details>
  <summary>How to generate a delegated payment address from a stake key (<strong>payment-delegated.addr</strong>)</summary>

```console
$ cardano-address address delegation $(cat stake.xvk) < payment.addr > payment-delegated.addr
addr_test1qp2fg770ddmqxxduasjsas39l5wwvwa04nj8ud95fde7f70k6tew7wrnx0s4465nx05ajz890g44z0kx6a3gsnms4c4qq8ve0n
```
</details>

<details>
  <summary>How to generate a stake address from a stake key (<strong>stake.addr</strong>)</summary>

```console
$ cardano-address address stake --network-tag testnet < stake.xvk > stake.addr
stake_test1urmd9uh08pen8c26a2fn86weprjh52638mrdwc5gfac2u2s25zpat%
```
</details>

<details>
  <summary>How to generate a payment verification key for shared wallet (<strong>addr_shared.vk</strong>, <strong>stake_shared.vk</strong>)</summary>

Let's generate extended root private key for shared style:

``` console
$ cardano-address key from-recovery-phrase Shared < phrase.prv > root_shared.xsk
```

Now generate payment verification key (`role=0` is used). Please note that purpose `1854H` is used for multisig.

```console
$ cardano-address key child 1854H/1815H/0H/0/0 < root_shared.xsk | cardano-address key public --without-chain-code > addr_shared.vk
addr_shared_vk1a9h46rvjnqquxz02zyesh0ct29szh7vv9x7r2h87ttmnkgrfgguqhz0mtc
```

Generating delegation verification key is the similar (the only difference is role=2)

```console
$ cardano-address key child 1854H/1815H/0H/2/0 < root_shared.xsk | cardano-address key public --without-chain-code > stake_shared.vk
stake_shared_vk18a8z5dcrlwene88n84j6dm9yvj5rt296fjtresqnunmacetdcymquyq43z
```

> :information_source: The last segment in the path is the key index, which can be incremented to derive more keys. Up `2^31-1` keys are possible.
</details>

<details>
  <summary>How to construct a multisig script hash (<strong>script.hash</strong>)</summary>

We consider `addr_shared.1.vk` and `addr_shared.2.vk` obtained like `addr_shared.vk` but by replacing the final index by `1` and `2` respectively.

```console
$ cardano-address script hash "all [$(cat addr_shared.1.vk), $(cat addr_shared.2.vk)]" > script.hash
script1gr69m385thgvkrtspk73zmkwk537wxyxuevs2u9cukglvtlkz4k
```

This script requires the signature from both signing keys corresponding to `shared_addr.1.vk` and `shared_addr.2.vk` (ie., shared_addr.1.sk and shared_addr.2.sk) in order to be valid. Similarly, we could require only one of the two signatures:

We can also use extended verification, eiher payment or delegation, keys. They can be obtained as the non-extended ones by using `--with-chain-code` option rather than `--without-chain-option` as above. They will give rise to the same script hash as for verification keys chain code is stripped upon calculation.

```console
$ cardano-address script hash "any [$(cat addr_shared.1.xvk), $(cat addr_shared.2.xvk)]"
script1gr69m385thgvkrtspk73zmkwk537wxyxuevs2u9cukglvtlkz4k
```

which is equivalent (functionally, but not in terms of hash value) to :

```console
$ cardano-address script hash "at_least 1 [$(cat addr_shared.1.xvk), $(cat addr_shared.2.xvk)]"
script13uf3fz3ts5srpjc5zcfe977uvnyvp36wcvxuudryegz0zpjlx6a
```
</details>

<details>
  <summary>How to construct a multisig script hash with timelocks</summary>

```console
$  cardano-address script hash "all [$(cat addr_shared.1.xvk), $(cat addr_shared.2.xvk), active_from 100, active_until 120]"
```
</details>


<details>
  <summary>How to validate a script</summary>

```console
$  cardano-address script validate "at_least 1 [$(cat addr_shared.1.xvk), $(cat addr_shared.2.xvk), $(cat addr_shared.2.xvk)]"
Validated.

$  cardano-address script validate --recommended  "at_least 1 [$(cat addr_shared.1.xvk), $(cat addr_shared.2.xvk), $(cat addr_shared.2.xvk)]"
Not validated: The list inside a script has duplicate keys (which is not recommended)..
```
</details>

<details>
  <summary>How to get preimage for a script</summary>

```console
$ cardano-address script preimage "all [addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq, addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp]"
008201828200581c1196fe3062e96b78dd959058f5adad44dd663519200f2495d17ef10d8200581c2445facc08d975d9d965d360dbe0fa63688ccc8f70fd7e1f01135380

$  cardano-address script preimage "all [addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq, active_from 100, active_until 150]"
008201838200581c1196fe3062e96b78dd959058f5adad44dd663519200f2495d17ef10d8204186482051896
```
</details>

<details>
  <summary>How to generate a payment script address from a script hash (<strong>script.addr</strong>)</summary>

```console
$ cardano-address address payment --network-tag testnet < script.hash > script.addr
addr_test1wqqggtajwkxjgf58v452jz6jl87lt32w3mhez5hd7xz6hugp80tta
```
</details>

## Docker Image

### Build

```console
$ docker build -t cardano-address .
```

### Run

Use the auto-remove flag `--rm` when running commands.

```console
$ docker run --rm cardano-address recovery-phrase generate --size 15
dismiss grit bacon glare napkin satisfy tribe proud carpet bench fantasy rich history face north
```

Use the interactive flag `-i` when piping stdin

```console
$ echo "addr1gqtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevuph3wczvf2dwyx5u" | docker run --rm -i cardano-addresses address inspect
{
    "address_style": "Shelley",
    "stake_reference": "by pointer",
    "spending_key_hash": "1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3",
    "pointer": {
        "slot_num": 24157,
        "output_index": 42,
        "transaction_index": 177
    },
    "network_tag": 0
}
```

## Contributing

Pull requests are welcome.

When creating a pull request, please make sure that your code adheres to our
[coding standards](https://github.com/input-output-hk/adrestia/wiki/Coding-Standards).

<hr />

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-addresses/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-addresses.svg?style=for-the-badge" /></a>
</p>
