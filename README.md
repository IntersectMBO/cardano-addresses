<p align="center">
  <big><strong>Cardano Addresses</strong></big>
</p>

<p align="center">
  <a href="https://hackage.haskell.org/package/cardano-addresses"><img src="https://img.shields.io/hackage/v/cardano-addresses?style=for-the-badge" /></a>
  <a href="https://github.com/input-output-hk/cardano-addresses/releases"><img src="https://img.shields.io/github/v/release/input-output-hk/cardano-addresses?color=%239b59b6&label=RELEASE&sort=semver&style=for-the-badge"/></a>
  <a href='https://github.com/input-output-hk/cardano-addresses/actions?query=workflow%3A"Continuous Integration"'><img src="https://img.shields.io/github/workflow/status/input-output-hk/cardano-addresses/Continuous Integration?style=for-the-badge" /></a>
  <a href="https://input-output-hk.github.io/cardano-addresses/coverage/hpc_index.html"><img src="https://input-output-hk.github.io/cardano-addresses/coverage/badge.svg" /></a>
</p>

## Overview

This module provides mnemonic (backup phrase) creation, and conversion of a
mnemonic to seed for wallet restoration, and address derivation functionalities.

## Documentation

API documentation is available [here](https://input-output-hk.github.io/cardano-addresses/haddock).

## Command-Line 

`cardano-address` comes with a command-line interface for Linux. See the [release artifacts](https://github.com/input-output-hk/cardano-addresses/releases) to get a pre-compiled binary. The command-line is self explanatory by using `--help` on various commands and sub-commands. 

> :bulb: Most commands read argument from the standard input. This prevent sensitive information from appearing into your shell history and, makes it easy to pipe commands!

Here's are some key examples:

<details>
  <summary>How to generate a recovery phrase</summary>

```console
$ cardano-address recovery-phrase generate --size 15
east student silly already breeze enact seat trade few way online skin grass humble electric
```
</details>

<details>
  <summary>How to generate a root private key</summary>

```console
$ cardano-address recovery-phrase generate --size 15 > recovery-phrase.prv
$ cat recovery-phrase.prv | cardano-address key from-recovery-phrase Shelley
xprv1fzu4e8cecxshgzzxzh7557sd8tffqreeq2je7fgsm7f02mq849vdupw7qwgxc3qawyqev0l8ew0f4fkp8hvr8mskz4hz6e6ejzjlevcskcl6lqpr07u7552fsfgteztuclse7luh4cp493zdhkrjdss0250cdw8n
```

Notice the `xprv` prefix to identify an e**x**tended **prv**ivate key. Should you prefer an hexadecimal output, you can choose a different output encoding via a special flag:

```console
$ cat recovery-phrase.prv | cardano-address key from-recovery-phrase Shelley --base16
48b95c9f19c1a174084615fd4a7a0d3ad2900f3902a59f2510df92f56c07a958
de05de03906c441d7101963fe7cb9e9aa6c13dd833ee16156e2d675990a5fcb3
10b63faf80237fb9ea51498250bc897cc7e19f7f97ae0352c44dbd8726c20f55
```
</details>

<details>
  <summary>How to generate a public stake key</summary>

```console
$ cardano-address recovery-phrase generate --size 15 > recovery-phrase.prv
$ cat recovery-phrase.prv \
  | cardano-address key from-recovery-phrase Shelley \
  | cardano-address key child 1852H/1815H/0H/2/0 \
  | cardano-address key public 
xpub16y4vhpyuj2t84gh2qfe3ydng3wc37yqzxev6gce380fvvg47ye8um3dm3wn5a64gt7l0fh5j6sjlugy655aqemlvk6gmkuna46xwj9g4frwzw
```

> :information_source: `1852H/1815H/0H/2/0` is the derivation path that is typically used by Cardano wallet to identify a stake key within HD wallet. If you seek compatibility with Daedalus or Yoroi, use this as well!

</details>


## Contributing

Pull requests are welcome.

When creating a pull request, please make sure that your code adheres to our
[coding standards](https://github.com/input-output-hk/adrestia/wiki/Coding-Standards).

<hr />

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-addresses/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-addresses.svg?style=for-the-badge" /></a>
</p>
