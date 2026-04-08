# Quickstart: WASM Build

## Prerequisites

```bash
# Get GHC WASM toolchain via nix
nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12'
```

## Build for WASM

```bash
# One-time: disable shared linking in WASM cabal config
sed -i 's/^shared: True/shared: False/' ~/.ghc-wasm/.cabal/config

# Build
wasm32-wasi-cabal --project-file=cabal-wasm.project build cardano-addresses
```

## Build for native (unchanged)

```bash
cabal build cardano-addresses
cabal test unit
```

## Verify WASM artifact

```bash
# Check the .wasm output exists
find dist-newstyle -name '*.wasm' -type f
```
