<p align="center">
  <big><strong>Cardano Addresses</strong></big>
</p>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-addresses/releases"><img src="https://img.shields.io/github/v/release/input-output-hk/cardano-addresses?color=%239b59b6&label=RELEASE&sort=semver&style=for-the-badge"/></a>
  <a href='https://github.com/input-output-hk/cardano-addresses/actions?query=workflow%3A"Continuous Integration"'><img src="https://img.shields.io/github/workflow/status/input-output-hk/cardano-addresses/Continuous Integration?style=for-the-badge" /></a>
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

Here's are some key examples:

<details>
  <summary>How to generate a recovery phrase</summary>

```
$ cardano-address recovery-phrase generate --size 15 > recovery-phrase.txt
east student silly already breeze enact seat trade few way online skin grass humble electric
```
</details>

<details>
  <summary>How to generate a root private key</summary>

```
$ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley
root_xsk1zz8x2k6jemuq884k4s4c862n2maaskk07ua7xc4pcegfd70fx9ymnhrk5jkex5rh5fggph0682rdxjpaf97lkrmqqs49md3mq0n5ds4e07en8xnyn3g3f85zn85gapcwkht6y5djqrjmqdnqp2rg5nvmyc99drac
```

Notice the `root_xsk` prefix to identify a root extended signing (private) key.
</details>

<details>
  <summary>How to generate an extended public stake key (public key plus chain code)</summary>

```
$ cardano-address recovery-phrase generate --size 15 > recovery-phrase.txt
$ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
| cardano-address key child 1852H/1815H/0H/2/0 \
| cardano-address key public --with-chain-code
stake_xvk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7yak6lmcyst8yclpm3yalrspc7q2wy9f6683x6f9z4e3gclhs5snslcst62
```
Notice the `stake_xvk` prefix to identify a stake extended verification (public) key.

> :information_source: `1852H/1815H/0H/2/0` is the derivation path that is typically used by Cardano wallet to identify a stake key within HD wallet. If you seek compatibility with Daedalus or Yoroi, use this as well!

</details>

<details>
  <summary>How to generate a public stake key (public key without chain code)</summary>

```
$ cardano-address recovery-phrase generate --size 15 > recovery-phrase.txt
$ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
| cardano-address key child 1852H/1815H/0H/2/0 \
| cardano-address key public --without-chain-code
stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d
```
Notice the `stake_vk` prefix to identify a stake verification (public) key.

> :information_source: `1852H/1815H/0H/2/0` is the derivation path that is typically used by Cardano wallet to identify a stake key within HD wallet. If you seek compatibility with Daedalus or Yoroi, use this as well!

</details>

<details>
  <summary>How to generate a payment address from key credential</summary>

```
  $ cardano-address recovery-phrase generate --size 15 \
  | cardano-address key from-recovery-phrase Shelley | tee root.prv

  root_xsk1fqf5s0s7mluhs8q8xduvhy9rgxgj90x8yg0ht92ymhr775k3uap4zxvh4fzcqtg7j9ewvag3lsgswryav3w650gul4mza65nrp6vdx5dt8xelgx87avxcwfqwkajm7masntd3ef3duwj39pnacupl68u4yw4ss9h

  $ cat root.prv \
  | cardano-address key child 1852H/1815H/0H/0/0 | tee addr.prv

  addr_xsk1krnkg5uyxgdxxpkzrwscr99d0ln9dylg84qspktsvgj42c73uaplrzymsrvef3qsq0mqy8hjxk9m00k9yexym3578lxzszfanmr83cu9jhm9nzmwnfnps36gcz4d328cz49eqtgq4whl2ev402f5ae0ht5kenmd2

  $ cat addr.prv \
  | cardano-address key public --with-chain-code \
  | cardano-address address payment --network-tag testnet

  addr_test1vq3a2mkasqfmsc5y5tguuckqaxa3pafrxs9x26d2hqvjehc8dk63q
```
</details>

<details>
  <summary>How to generate a delegation address from stake key credentials</summary>

  Follow the steps from 'How to generate a payment address'. Then, simply extend
  an existing payment address with a stake key!

```
  $ cat root.prv \
  | cardano-address key child 1852H/1815H/0H/2/0 \
  | tee stake.prv

  stake_xsk1pr95hal8lfp4wnq4m9zmvv9emy9hakx0h73qh5zkzeprxek3uapayt7qhelgfgvxx40wqr0g9j5rfwg56ujvlc5thfhrctkqld3gegyc24jf0jp59qkk8xcgqztdp762wmcx5km4w8duh77xjfx6m3yw6qw65q6w

  $ cat addr.prv \
  | cardano-address key public --with-chain-code \
  | cardano-address address payment --network-tag testnet \
  | cardano-address address delegation $(cat stake.prv | cardano-address key public --with-chain-code)

  addr_test1qq3a2mkasqfmsc5y5tguuckqaxa3pafrxs9x26d2hqvjehesytwfe2p0tt06y5ptxwvvm6lfu255dny7yz8e9kv4e83qwwzy29
```
</details>

<details>
  <summary>How to construct a script and calculate its hash</summary>

  Let's say I have the following recovery phrase
```
$ cat recovery-phrase.txt
east student silly already breeze enact seat trade few way online skin grass humble electric
```
 Now we create root key
```
$ cat recovery-phrase.txt \
| cardano-address key from-recovery-phrase Shelley \
| tee root.xpr

root_xsk1zz8x2k6jemuq884k4s4c862n2maaskk07ua7xc4pcegfd70fx9ymnhrk5jkex5rh5fggph0682rdxjpaf97lkrmqqs49md3mq0n5ds4e07en8xnyn3g3f85zn85gapcwkht6y5djqrjmqdnqp2rg5nvmyc99drac
```
 And derive two signing keys according to multisig CIP (TO-DO add link when available)
```
$ cat root.xprv \
| cardano-address key child 1852H/1815H/0H/3/0 \
| tee signingKey1.xprv

script_xsk1tr75z5eem5gxxredy7qj63v95vudfwfrmd0cd0772exekr02x9y6nhnranafyws2d0e5r0du87f0llu4dpdwsrlgxafwlwv7vzsanwa79jjj269v36uqekx368myrrqxe4u86dw0t0ncnjjn6wc8w57pmu5gdsr4

$ cat root.xprv \
| cardano-address key child 1852H/1815H/0H/3/1 \
| tee signingKey2.xprv

script_xsk1krdfq60htlc58s6gq4uesytc74vzsa99d9yzpwlyhd2gwzl2x9ya5vhfkt4yzqu4k3v7h3kr00ey983n4er2sl3eftds28hegx5f5hq6x73l4v26e08jcyq3dhwdru6fq9nc54evkktec0hgh5dw7g0uqvnhm24p
```
 The corresponding verification keys and their hashes can be obtained as follows
```
$ cat signingKey1.xprv \
| cardano-address key public --without-chain-code \
| cardano-address key hash \
| tee verKey1.hash

script_vkh15thv7uqqd0jh48fv4achd7ufqu9kv98c78r40q7fpwylw2c6wft

$ cat signingKey2.xprv \
| cardano-address key public --without-chain-code \
| cardano-address key hash \
| tee verKey2.hash

script_vkh18w06sur9ll7pcw8nzzr9mllgzyrrraaqse8336k2ej2cyph0u2a
```
 Notice the `script_vkh` prefix to identify the hash of verification key.

 Now we can construct the script using the hashes of verification keys
```
$ echo "all [$(cat verKey1.hash),$(cat verKey2.hash)]" | tee script.txt

all [script_vkh15thv7uqqd0jh48fv4achd7ufqu9kv98c78r40q7fpwylw2c6wft,script_vkh18w06sur9ll7pcw8nzzr9mllgzyrrraaqse8336k2ej2cyph0u2a]
```
 Having a script constructed we can get its script hash that could go to payment or delegation credential
 when creating the address. Notice the `script` prefix to identify the hash of the script.

```
$ cardano-address script hash "$(cat script.txt)"

script1qd46g66dq98dh8h2qh947wq3qajf3plmlnkkedn2yexxke2lahd
```
</details>

<details>
  <summary>How to generate a payment address from script credential</summary>

```
  $ cardano-address script hash "$(cat script.txt)" \
  | cardano-address address payment --network-tag testnet

  addr_test1wqpkhfrtf5q5aku7agzukheczyrkfxy8l07w6m9kdgnyc6crtxw6n
```
</details>

<details>
  <summary>How to generate a delegation address from script key credentials having key payment</summary>

  Follow the steps from 'How to generate a payment address'. Then, simply extend
  an existing payment address with a stake key!

```
  $ cat root.prv \
  | cardano-address key child 1852H/1815H/0H/2/0 > stake.prv

  $ cat addr.prv \
  | cardano-address key public --with-chain-code \
  | cardano-address address payment --network-tag testnet \
  | cardano-address address delegation $(cardano-address script hash "$(cat script.txt)")
  addr_test1yqqc24zex4mqch3hp5q7da87mwufkl7hncg472phe74ea2...
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
