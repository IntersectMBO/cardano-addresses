# Research: WASM Build Target

**Date**: 2026-04-08
**Branch**: `001-wasm-target`

## R1: Can `ram` + `crypton` compile for WASM?

**Decision**: Yes, both compile for `wasm32-wasi` with specific flags.

**Rationale**: Experimentally verified on 2026-04-08:
- `ram` 0.22.0 compiles with `-D_WASI_EMULATED_MMAN -lwasi-emulated-mman` (mmap emulation) and `shared: False`
- `crypton` 1.1.2 compiles with `-DARGON2_NO_THREADS` (disable pthreads in argon2)
- All 26 `ram` modules and all `crypton` modules compile successfully
- Linker succeeds when dynamic linking is disabled (`shared: False` in cabal config)

**Alternatives considered**:
- Patching `basement` for GHC 9.12: `GHC.IntWord64` removal requires 5+ fixes, 32-bit code paths need audit, package is archived — rejected as unmaintainable
- Patching `memory`: same `basement` dependency, same problems — rejected

## R2: What does vendoring `Cardano.Crypto.Wallet` require?

**Decision**: ~565 lines, 2 trivial import fixes, plus C sources.

**Rationale**: Audit of cardano-crypto 1.3.0 source:
- `Wallet.hs` (202 lines): imports `Basement.Compat.Typeable` → replace with `Data.Typeable`
- `Wallet/Types.hs` (29 lines): imports `Foundation`/`Foundation.Check`/`Foundation.Collection` → only for `Arbitrary` instance, drop or rewrite with QuickCheck
- `Wallet/Encrypted.hs` (226 lines): zero basement imports, uses `Data.ByteArray` (from `ram`)
- `Wallet/Pure.hs` (108 lines): zero basement imports, uses `Data.ByteArray` (from `ram`)
- C sources needed: `cbits/ed25519/` (ed25519-donna), `cbits/encrypted_sign.c` — portable C, should compile with wasi-sdk

**Alternatives considered**:
- Using `cardano-crypto` directly with patched basement: fragile, unmaintainable — rejected
- Reimplementing wallet ops from scratch: unnecessary since source is available under Apache-2.0 — rejected

## R3: What does vendoring `Crypto.Encoding.BIP39` require?

**Decision**: ~600 lines of logic + 2081-line word list, requires replacing 7 basement types.

**Rationale**: Audit of cardano-crypto 1.3.0 BIP39 modules:

| Basement type | Used for | Replacement |
|---|---|---|
| `ListN n a` | Length-indexed mnemonic sentence | `[a]` with runtime length check, or vendored `ListN` (~80 lines) |
| `Basement.String` | BIP39 word representation | `Text` |
| `Offset String` / `WordIndex` | Dictionary word index | `Word16` newtype |
| `NormalForm` | Deep evaluation | `NFData` from `deepseq` |
| `NatWithinBound` | Type-level nat constraint | `KnownNat` + manual bound check |
| `TryFrom` | Partial conversion class | Direct `Maybe`-returning functions |
| `Vect 2048 String` | English word dictionary | `Vector Text` |

The `ListN` type is the most structural — it's used in the public API types `MnemonicSentence` and `MnemonicPhrase`. Options:
1. Keep `ListN` by vendoring ~80 lines from basement (it's just a newtype over `[]` with phantom length)
2. Replace with plain `[]` and add runtime length validation

Option 1 preserves the existing API better and is less disruptive to `Cardano.Mnemonic`.

**Alternatives considered**:
- Using `ppad-bip39`: clean alternative but depends on `text-icu` (C FFI to ICU library), adds a new C dependency for WASM — rejected for now
- Keeping basement just for BIP39: defeats the purpose — rejected

## R4: Does `Data.ByteArray` from `ram` have the same API as from `memory`?

**Decision**: Yes, drop-in replacement.

**Rationale**: `ram` is a maintained fork of `memory` that removes the `basement` dependency. It exports identical module paths (`Data.ByteArray`, `Data.ByteArray.Types`, etc.) and the same types (`ScrubbedBytes`, `ByteArrayAccess`, `Bytes`, `convert`, etc.).

**Alternatives considered**: None needed — `ram` is the direct successor.

## R5: Can cardano-crypto's C sources compile for WASM?

**Decision**: Very likely yes, needs verification.

**Rationale**:
- `ed25519-donna` implementation includes `ed25519-donna-portable.h` — generic C, no platform-specific assembly
- `encrypted_sign.c` is straightforward C with no OS-specific APIs
- Both use only `<string.h>`, `<stdint.h>`, and `Data.ByteArray` FFI — all available under wasi-sdk

**Risk**: The `ed25519-donna` code has architecture detection headers that select between 32-bit and 64-bit implementations. WASM is 32-bit, so the 32-bit path will be taken. This is portable but slower.

## R6: Template Haskell on WASM

**Decision**: Potential blocker, needs investigation during implementation.

**Rationale**: `bech32-th` uses Template Haskell for compile-time bech32 prefix validation. GHC's WASM backend supports TH via an external interpreter (running splices on the host), but this requires specific setup. If it fails, the TH usage can be replaced with runtime validation.

**Risk**: Medium. TH via external interpreter is fragile. Fallback is to make `bech32-th` dependency conditional on `arch(wasm32)`.
