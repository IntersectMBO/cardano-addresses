# Implementation Plan: WASM Build Target

**Branch**: `001-wasm-target` | **Date**: 2026-04-08 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-wasm-target/spec.md`

## Summary

Eliminate the `basement`/`foundation`/`memory`/`cardano-crypto` dependency chain from cardano-addresses to enable compilation with GHC's WASM backend. Replace with `ram` (basement-free memory) + `crypton` 1.1.2 (uses ram) + vendored wallet and BIP39 modules.

## Technical Context

**Language/Version**: Haskell, GHC 9.6 (native CI), GHC 9.12 (WASM target)
**Primary Dependencies**: `crypton` >= 1.1.2 (crypto), `ram` >= 0.22 (byte arrays), `bech32`, `cborg`, `aeson`
**Storage**: N/A (pure library)
**Testing**: cabal test (hspec), 720 existing unit tests + WASM smoke test
**Target Platform**: Native (Linux/macOS/Windows) + wasm32-wasi
**Project Type**: Library
**Performance Goals**: Byte-identical output to native target
**Constraints**: No `basement`, `foundation`, `memory`, or `cardano-crypto` in dependency closure
**Scale/Scope**: ~7 files to modify, ~3200 lines to vendor

## Constitution Check

*No project-specific constitution defined. Proceeding with standard engineering practices.*

## Project Structure

### Documentation (this feature)

```text
specs/001-wasm-target/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output
```

### Source Code (repository root)

```text
lib/
├── Cardano/
│   ├── Address/
│   │   ├── Crypto.hs              # Existing facade (PR #394)
│   │   ├── Crypto/
│   │   │   └── Wallet.hs          # NEW: vendored from cardano-crypto
│   │   │   └── Wallet/
│   │   │       ├── Encrypted.hs   # NEW: vendored, C FFI for key ops
│   │   │       ├── Pure.hs        # NEW: vendored, pure key derivation
│   │   │       └── Types.hs       # NEW: vendored, XPrv/XPub types
│   │   ├── Derivation.hs          # Modified: import from vendored wallet
│   │   └── Style/
│   │       ├── Byron.hs           # Modified: import changes
│   │       └── Icarus.hs          # Modified: import changes
│   ├── Codec/
│   │   └── Cbor.hs                # Modified: import changes
│   └── Mnemonic.hs                # Modified: replace basement types
├── Crypto/
│   └── Encoding/
│       ├── BIP39.hs               # NEW: vendored, basement types replaced
│       └── BIP39/
│           ├── Dictionary.hs      # NEW: vendored, Offset -> Int
│           └── English.hs         # NEW: vendored, Vect -> Vector
cbits/
├── ed25519/                       # NEW: vendored from cardano-crypto
├── encrypted_sign.c               # NEW: vendored from cardano-crypto
└── ...
cabal-wasm.project                 # NEW: WASM build configuration
```

**Structure Decision**: Vendor modules into the existing `lib/` tree mirroring their original module paths. C sources go into `cbits/`. WASM config is a separate cabal project file.
