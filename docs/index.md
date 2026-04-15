---
sidebar_position: 1
title: Introduction
slug: /intro
---

<p align="center">
  <big><strong>Cardano Addresses</strong></big>
</p>

<p align="center">
  <a href="https://github.com/IntersectMBO/cardano-addresses/releases" rel="nofollow"><img src="https://img.shields.io/github/v/release/IntersectMBO/cardano-addresses?color=%239b59b6&label=RELEASE&sort=semver&style=for-the-badge" height="26"/></a>
  <a href="https://IntersectMBO.github.io/cardano-addresses/haddock/index.html" rel="nofollow"><img src="https://IntersectMBO.github.io/cardano-addresses/badges/haddock-badge.svg" height="26"/></a>
  <a href="https://IntersectMBO.github.io/cardano-addresses/hpc_index.html" rel="nofollow"><img src="https://IntersectMBO.github.io/cardano-addresses/badges/badge.svg" height="26"/></a>
  <a href="https://intersectmbo.github.io/cardano-addresses/intro/" rel="nofollow"><img src="https://IntersectMBO.github.io/cardano-addresses/badges/docusaurus-badge.svg" height="26"/></a>
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

![](../.github/example.gif)

## Documentation

## Command-Line

### Supported platforms

`cardano-addresses` is officially supported on the following operating systems:

- **Linux** - Ubuntu 20.04+, Debian 11+, Fedora 38+, and other major distributions
- **macOS** - version 11 (Big Sur) and later
- **Windows** - Windows 10 and Windows 11

`cardano-addresses` comes with CLI for Linux, MacOS and Windows. See [releases](https://github.com/IntersectMBO/cardano-addresses/releases) to get respective pre-compiled binaries. There is also straightforward way to [build Docker image](#docker-image). The command-line is self explanatory by using `--help` on various commands and sub-commands.

> :bulb: Most commands read argument from the standard input. This prevent sensitive information from appearing into your shell history and, makes it easy to pipe commands!

## Building/testing from source using nix

Prerequisites: [Install Nix](https://nixos.org/download.html) with flakes enabled.

### Enter development shell

``` console
nix develop
```

Inside the development shell:

``` console
# building
cabal build all

# testing
cabal test cardano-addresses:unit

# installing executable locally
cabal install cardano-address
```

### Build using nix directly

``` console
# Build the Linux x86_64 binary
nix build .

# Run the built binary
./result/bin/cardano-address
```

### Building for different platforms

``` console
# Linux x86_64
nix build .

# Darwin x86_64
nix build .#packages.x86_64-darwin.default

# Darwin aarch64 (Apple Silicon)
nix build .#packages.aarch64-darwin.default

# Linux aarch64
nix build .#packages.aarch64-linux.default

# Linux x86_64 to Windows (cross-compilation)
nix build .#packages.x86_64-linux.default
```

### Building the Docker image

``` console
nix build .#packages.x86_64-linux.docker-image
docker load < result
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

## WebAssembly

The library compiles to WebAssembly via GHC's WASM backend, producing a single `cardano-addresses.wasm` binary that runs in the browser or any WASI runtime.

### Build

```bash
nix build github:IntersectMBO/cardano-addresses#wasm
ls result/cardano-addresses.wasm   # 7.0MB
```

### Commands

The binary reads JSON from stdin and writes JSON to stdout. A `cmd` field selects the operation:

```bash
# Address inspection
echo '{"cmd":"inspect","address":"addr1..."}' | wasmtime result/cardano-addresses.wasm

# Key derivation (CIP-1852 Shelley)
echo '{"cmd":"derive","mnemonic":"word1 word2 ...","path":"1852H/1815H/0H/0/0"}' | wasmtime result/cardano-addresses.wasm

# Address construction
echo '{"cmd":"make-address","type":"enterprise","network":"testnet","payment_key":"hex..."}' | wasmtime result/cardano-addresses.wasm

# Ed25519 signing and verification
echo '{"cmd":"sign","key":"hex...","message":"hex..."}' | wasmtime result/cardano-addresses.wasm
echo '{"cmd":"verify","key":"hex...","message":"hex...","signature":"hex..."}' | wasmtime result/cardano-addresses.wasm

# Legacy bootstrap addresses (Byron/Icarus)
echo '{"cmd":"bootstrap-address","style":"icarus-from-mnemonic","protocol_magic":764824073,...}' | wasmtime result/cardano-addresses.wasm
```

### Browser integration

Use [@bjorn3/browser_wasi_shim](https://www.npmjs.com/package/@bjorn3/browser_wasi_shim) to run the WASM binary client-side:

```javascript
import { WASI, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim";

const mod = await WebAssembly.compile(await (await fetch("cardano-addresses.wasm")).arrayBuffer());

async function call(input) {
  let out = "";
  const fds = [
    new OpenFile(new File(new TextEncoder().encode(input))),
    ConsoleStdout.lineBuffered(l => out += l + "\n"),
    ConsoleStdout.lineBuffered(() => {}),
  ];
  const wasi = new WASI([], [], fds, { debug: false });
  wasi.start(await WebAssembly.instantiate(mod, { wasi_snapshot_preview1: wasi.wasiImport }));
  return JSON.parse(out.trim());
}
```

Benchmarked: ~9ms compile (one-time), ~3ms per Shelley call, ~13ms for legacy.

A live demo is available at: https://IntersectMBO.github.io/cardano-addresses/browser/

### Nix integration

Downstream flakes consume the WASM as a package:

```nix
{
  inputs.cardano-addresses.url = "github:IntersectMBO/cardano-addresses";

  outputs = { cardano-addresses, ... }: {
    packages.wasm = cardano-addresses.packages.x86_64-linux.wasm;
    # result/cardano-addresses.wasm
  };
}
```

## Contributing

Pull requests are welcome.

When creating a pull request, please make sure that your code adheres to our [coding standards](https://input-output-hk.github.io/adrestia/code/Coding-Standards).

