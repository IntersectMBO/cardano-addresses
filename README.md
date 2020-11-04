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
xprv1zz8x2k6jemuq884k4s4c862n2maaskk07ua7xc4pcegfd70fx9ymnhrk5jkex5rh5fggph0682rdxjpaf97lkrmqqs49md3mq0n5ds4e07en8xnyn3g3f85zn85gapcwkht6y5djqrjmqdnqp2rg5nvmycwgyqvx
```

Notice the `xprv` prefix to identify an e**x**tended **prv**ivate key. Should you prefer an hexadecimal output, you can choose a different output encoding via a special flag:

```
$ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley --base16
108e655b52cef8039eb6ac2b83e95356fbd85acff73be362a1c65096f9e93149
b9dc76a4ad935077a25080ddfa3a86d3483d497dfb0f60042a5db63b03e746c2
b97fb3339a649c51149e8299e88e870eb5d7a251b200e5b036600a868a4d9b26
```
</details>

<details>
  <summary>How to generate an extended public stake key (public key plus chain code)</summary>

```
$ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
| cardano-address key child 1852H/1815H/0H/2/0 \
| cardano-address key public --with-chain-code
xpub16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7yak6lmcyst8yclpm3yalrspc7q2wy9f6683x6f9z4e3gclhs5snst6nr2z
```

> :information_source: `1852H/1815H/0H/2/0` is the derivation path that is typically used by Cardano wallet to identify a stake key within HD wallet. If you seek compatibility with Daedalus or Yoroi, use this as well!

</details>

<details>
  <summary>How to generate a public stake key (public key without chain code)</summary>

```
$ cat recovery-phrase.prv | cardano-address key from-recovery-phrase Shelley \
| cardano-address key child 1852H/1815H/0H/2/0 \
| cardano-address key public --without-chain-code
xpub16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qh83kg9
```

> :information_source: `1852H/1815H/0H/2/0` is the derivation path that is typically used by Cardano wallet to identify a stake key within HD wallet. If you seek compatibility with Daedalus or Yoroi, use this as well!

</details>

<details>
  <summary>How to generate a payment address from key credential</summary>

```
  $ cardano-address recovery-phrase generate --size 15 \
  | cardano-address key from-recovery-phrase Shelley > root.prv

  $ cat root.prv \
  | cardano-address key child 1852H/1815H/0H/0/0 > addr.prv

  $ cat addr.prv \
  | cardano-address key public --with-chain-code \
  | cardano-address address payment --from-key --network-tag testnet

  addr_test1vqrlltfahghjxl5sy5h5mvfrrlt6me5fqphhwjqvj5jd88cccqcek
```
</details>

<details>
  <summary>How to generate a delegation address from stake key credentials</summary>

  Follow the steps from 'How to generate a payment address'. Then, simply extend
  an existing payment address with a stake key!

```
  $ cat root.prv \
  | cardano-address key child 1852H/1815H/0H/2/0 > stake.prv

  $ cat addr.prv \
  | cardano-address key public --with-chain-code \
  | cardano-address address payment --from-key --network-tag testnet \
  | cardano-address address delegation --from-key $(cat stake.prv | cardano-address key public --with-chain-code)
  addr1vrcmygdgp7v3mhz78v8kdsfru0y9wysnr9pgvvgmdqx2w0qrg8swg...
```
</details>

<details>
  <summary>How to construct a script and calculate its hash</summary>

  Let's say I have the following recovery phrase
```
$ cat recovery-phrase.txt
nothing heart matrix fly sleep slogan tomato pulse what roof rail since plastic false enlist
```
 Now we create root key
```
$ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley > root.xprv
xprv1apjwjs3ksgm5mnnk0cc5v5emgv0hmafmmy8tffay5s2ffk69830whwznr46672ruucdzwwtv9upv72e4ylrypyz5m6cyh0p00t7n3u3agt20lv32j4kxcqlkzu78nzjx0ysxxlc2ghfz9prxfmrds802xsh67k7t
```
 And derive two signing keys according to multisig CIP (TO-DO add link when available)
```
$ cat root.xprv | cardano-address key child 1852H/1815H/0H/3/0 > signingKey1.xprv
xprv1nrywvqyuwefulu6mqmpxelz92kcy0zluu5kf5p74c6l6f6z9830pfvcvgen923akys2d2fmlh8t3fttj6nvx040k30ek8k62jyvv9p38c8lyuz9qkyhgnyme7ay742e7nemd037jdevpl688fdvtcfq9eqpnkr57
$ cat root.xprv | cardano-address key child 1852H/1815H/0H/3/1 > signingKey2.xprv
xprv1vpr59y3p3cfggk85x6dvmlpkwm9f4c99lvkmw8r6j5vwd669830rw5lvsuh8530q897ht9a297kw2qhkpvn7kk98njhdg3scter64uukrwtashzde54v8ery8a92npx4e22ffg45mtshe6ewpnzjx2cn9qs42k0f
```
 The corresponding verification keys and their hashes can be obtained as follows
```
$ cat signingKey1.xprv | cardano-address key public --with-chain-code | cardano-address key hash --base16 > verKey1.hash
de5861cd05e99985b2c586ab383790c6600990809206f84e96eadaea
$ cat signingKey2.xprv | cardano-address key public --with-chain-code | cardano-address key hash --base16 > verKey2.hash
aca52d7d28ce353f4766e4e2c8cc2208c7113d794e776eafb8c07a80
```
Also notice the default hrp (in bech32) for the hashes of verification key - **xpub_hash**
```
$ cat signingKey1.xprv | cardano-address key public --with-chain-code | cardano-address key hash
xpub_hash1mevxrng9axvctvk9s64nsduscesqnyyqjgr0sn5katdw5egajw2
```
 Now we can construct the script using the hashes of verification keys
```
$ echo "all [$(cat verKey1.hash),$(cat verKey2.hash)]" > script.txt
all [de5861cd05e99985b2c586ab383790c6600990809206f84e96eadaea,aca52d7d28ce353f4766e4e2c8cc2208c7113d794e776eafb8c07a80]
```
 Having a script constructed we can get its script hash that could go to payment or delegation credential
 when creating the address. The default hrp (in bech32) is **script_hash**

```
$ cardano-address script hash "$(cat script.txt)"
script_hash15hx806zf0g8kcv399dpxf6fq4l98myqpvvzj2rltg465uz36435
```
</details>

<details>
  <summary>How to generate a payment address from script credential</summary>

```
  $ cardano-address script hash "$(cat script.txt)" \
  | cardano-address address payment --from-script --network-tag testnet
  addr_test1wzjucalgf9aq7mpjy545ye8fyzhu5lvsq93s2fg0adzh2nsxaqdy4
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
  | cardano-address address payment --from-key --network-tag testnet \
  | cardano-address address delegation --from-script $(cardano-address script hash "$(cat script.txt)")
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
