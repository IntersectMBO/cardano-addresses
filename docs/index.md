---
sidebar_position: 1
title: Introduction
slug: /intro
---

<p align="center">
  <big><strong>Cardano Addresses</strong></big>
</p>

<p align="center">
  <a href="https://github.com/IntersectMBO/cardano-addresses/releases"><img src="https://img.shields.io/github/v/release/IntersectMBO/cardano-addresses?color=%239b59b6&label=RELEASE&sort=semver&style=for-the-badge"/></a>
  <a href="https://IntersectMBO.github.io/cardano-addresses/coverage/hpc_index.html"><img src="https://IntersectMBO.github.io/cardano-addresses/coverage/badge.svg" /></a>
  <br />
</p>


<div align="center">

  <a href="">[![Coding Standards](https://github.com/IntersectMBO/cardano-addresses/actions/workflows/style.yml/badge.svg?branch=master)](https://github.com/IntersectMBO/cardano-addresses/actions/workflows/style.yml)</a>
  <a href="">[![Haskell CI using Cabal](https://github.com/IntersectMBO/cardano-addresses/actions/workflows/haskell.yml/badge.svg)](https://github.com/IntersectMBO/cardano-addresses/actions/workflows/haskell.yml)</a>

</div>

## Overview

This module provides mnemonic (backup phrase) creation, and conversion of a
mnemonic to seed for wallet restoration, and address derivation functionalities.

![](../.github/example.gif)

## Documentation

API documentation is available [here](https://IntersectMBO.github.io/cardano-addresses/haddock).

## Command-Line

`cardano-address` comes with a command-line interface for Linux. See the [release artifacts](https://github.com/IntersectMBO/cardano-addresses/releases) or [continuous integration artifacts](https://github.com/IntersectMBO/cardano-addresses/actions?query=workflow%3A%22Continuous+Integration%22) to get a pre-compiled binary, or [build a Docker image](#docker-image). The command-line is self explanatory by using `--help` on various commands and sub-commands.

> :bulb: Most commands read argument from the standard input. This prevent sensitive information from appearing into your shell history and, makes it easy to pipe commands!

