
<p align="center">
  <big><strong>Cardano Addresses</strong></big>
</p>

<p align="center">
  <a href="https://github.com/IntersectMBO/cardano-addresses/releases" rel="nofollow"><img src="https://img.shields.io/github/v/release/IntersectMBO/cardano-addresses?color=%239b59b6&label=RELEASE&sort=semver&style=for-the-badge" height="26"/></a>
  <a href="https://IntersectMBO.github.io/cardano-addresses/haddock/index.html" rel="nofollow"><img src="https://IntersectMBO.github.io/cardano-addresses/coverage/haddock-badge.svg" height="26"/></a>
  <a href="https://IntersectMBO.github.io/cardano-addresses/coverage/hpc_index.html" rel="nofollow"><img src="https://IntersectMBO.github.io/cardano-addresses/coverage/badge.svg" height="26"/></a>
  <a href="https://intersectmbo.github.io/cardano-addresses/intro/" rel="nofollow"><img src="https://IntersectMBO.github.io/cardano-addresses/coverage/docusaurus-badge.svg" height="26"/></a>
  <br />
</p>

<div align="center">

  <a href="">[![Coding Standards](https://github.com/IntersectMBO/cardano-addresses/actions/workflows/style.yml/badge.svg?branch=master)](https://github.com/IntersectMBO/cardano-addresses/actions/workflows/style.yml)</a>
  <a href="">[![Haskell CI using Cabal](https://github.com/IntersectMBO/cardano-addresses/actions/workflows/haskell.yml/badge.svg)](https://github.com/IntersectMBO/cardano-addresses/actions/workflows/haskell.yml)</a>
  <a href="">[![Docs](https://github.com/IntersectMBO/cardano-addresses/actions/workflows/docs.yml/badge.svg)](https://github.com/IntersectMBO/cardano-addresses/actions/workflows/docs.yml)</a>
  <a href="https://docusaurus.io/"><img src="https://img.shields.io/badge/Docusaurus-3.7.0-blue?logo=docusaurus"/></a>

</div>


## Overview

This module provides mnemonic (backup phrase) creation, and conversion of a
mnemonic to seed for wallet restoration, and address derivation functionalities.

![](.github/example.gif)

## Documentation

### Haddock documentation

Haddock API documentation is available [here](https://IntersectMBO.github.io/cardano-addresses/haddock/index.html).

### Docusaurus-powered documentation

Powered by <a href="https://docusaurus.io/"><img src="https://images.icon-icons.com/2699/PNG/512/docusaurus_logo_icon_171229.png" width="64" alt="Docusaurus logo"/></a>

CLI documentation is available [here](https://IntersectMBO.github.io/cardano-addresses)

### Supported platforms

cardano-addresses is officially supported on the following operating systems:

- **Linux** - Ubuntu 20.04+, Debian 11+, Fedora 38+, and other major distributions
- **macOS** - version 11 (Big Sur) and later
- **Windows** - Windows 10 and Windows 11

cardano-addresses comes with CLI for Linux, MacOS and Windows. See [releases](https://github.com/IntersectMBO/cardano-addresses/releases) to get respective pre-compiled binaries. There is also straightforward way to [build Docker image](#docker-image).

## Building/testing from source using nix

``` console
nix develop

# building
cabal build all

# testing
cabal test cardano-addresses:unit

# installing executable locally
cabal install cardano-address
```

## Override command for cross-compilation

We have now fixed cross-compilation (from Linux to Windows) by replacing runtime `git` call in `System.Git.TH` with CPP macro (ie., `GITREV`) defaulting to "unknown" but allowing overriding via `-DGITREV` as below:

```console
cabal build all --ghc-option=-DGITREV=\"$(git rev-parse HEAD)\"
```

## Preparation steps before uploading to hackage

``` console
cabal build all
cabal haddock
cabal sdist
```

Note: Make sure proper version is set in cardano-addresses.cabal

## Docker Image

Please make sure you have [just](https://github.com/casey/just) installed as `justfile` is used for building Docker image.

### Build

```console
just clean-build-docker
```

### Run

Use the auto-remove flag `--rm` when running commands.

```console
docker run --rm cardano-address recovery-phrase generate --size 15
```

Use the interactive flag `-i` when piping stdin:

```console
echo "addr1gqtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevuph3wczvf2dwyx5u" | docker run --rm -i cardano-addresses address inspect
```

## Javascript support

Javascript support was discontinued and dropped. One could look at the following now:

1. [MeshJS](https://github.com/MeshJS/mesh)
2. [blaze-cardano](https://github.com/butaneprotocol/blaze-cardano)

Alternatively one could lean back on release [3.9.0](https://github.com/IntersectMBO/cardano-addresses/releases/tag/3.9.0) where Javascript was still present.

## Docs generation

The README.md is generated from this file using:
```
just generate-readme
```

## Contributing

Pull requests are welcome.

When creating a pull request, please make sure that your code adheres to our
[coding standards](https://input-output-hk.github.io/adrestia/code/Coding-Standards).

## macOS Code Signing and Notarization

To release macOS executables, you need to configure Apple code signing and notarization secrets in GitHub.

### Required Secrets (GitHub > Settings > Secrets and variables > Actions)

| Secret | Description | How to Obtain |
|--------|-------------|---------------|
| `APPLE_CERTIFICATE` | Base64-encoded p12 certificate | 1. Create an Apple Developer account<br>2. Generate a certificate signing request<br>3. Create "Developer ID Application" certificate in Apple Developer portal<br>4. Export certificate as p12 (include private key)<br>5. Run `base64 -w0 certificate.p12` to encode |
| `APPLE_CERTIFICATE_PASSWORD` | Password for the p12 certificate | Password you set when exporting the p12 certificate |
| `APPLE_ID_USERNAME` | Apple ID email for notarization | Your Apple Developer account email |
| `APPLE_ID_PASSWORD` | App-specific password | Generate at https://appleid.apple.com (Sign in > Security > App-Specific Passwords) |

### Required Variables (GitHub > Settings > Variables)

| Variable | Description | How to Obtain |
|----------|-------------|---------------|
| `APPLE_TEAM_ID` | Your Apple Team ID | Find in Apple Developer portal (Membership details) |

### Steps to Configure

1. Create an Apple Developer account if you don't have one
2. Create a Developer ID Application certificate (not for App Store)
3. Export the certificate as p12 file
4. Encode it: `base64 -w0 certificate.p12`
5. Create the secrets in GitHub repository settings
6. Generate an App-Specific Password at appleid.apple.com for notarization

>>>>>>> 6038a4fd (docs: add macOS notarization secrets documentation)
<hr />

<p align="center">
  <a href="https://github.com/IntersectMBO/cardano-addresses/blob/master/LICENSE"><img src="https://img.shields.io/github/license/IntersectMBO/cardano-addresses.svg?style=for-the-badge" /></a>
</p>

