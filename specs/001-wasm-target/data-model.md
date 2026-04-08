# Data Model: WASM Build Target

**Date**: 2026-04-08

## Vendored Types

### From `Cardano.Crypto.Wallet`

These types move from the `cardano-crypto` package into `cardano-addresses` directly. No structural changes — only import sources change.

| Type | Description | Size |
|---|---|---|
| `XPrv` | Extended private key (opaque, 128 bytes: 64 prv + 32 pub + 32 cc) | Existing |
| `XPub` | Extended public key (32-byte pub + 32-byte chain code) | Existing |
| `XSignature` | Ed25519 signature (64 bytes) | Existing |
| `ChainCode` | 32-byte chain code newtype | Existing |
| `DerivationScheme` | Enum: `DerivationScheme1` or `DerivationScheme2` | Existing |

### From `Crypto.Encoding.BIP39`

These types change their internal representation when vendored:

| Original type | Original impl | Vendored replacement |
|---|---|---|
| `MnemonicSentence mw` | `newtype ListN mw WordIndex` | `newtype ListN mw WordIndex` (vendored ListN) |
| `MnemonicPhrase mw` | `newtype ListN mw String` | `newtype ListN mw Text` |
| `WordIndex` | `newtype Offset String` | `newtype Word16` |
| `Dictionary` | Record with `Vect 2048 String` | Record with `Vector Text` |
| `Entropy ent` | Uses `Basement.Nat` constraints | Uses `GHC.TypeLits` constraints |

### Vendored `ListN`

Minimal sized-list type extracted from basement (~80 lines):

| Type | Description |
|---|---|
| `ListN (n :: Nat) a` | Newtype over `[a]` with phantom length parameter |
| `toListN :: KnownNat n => [a] -> Maybe (ListN n a)` | Construct with runtime length check |
| `unListN :: ListN n a -> [a]` | Unwrap |
| `mapM`, `foldl'`, `map` | Standard operations preserving length |

## Dependency Changes

| Before | After |
|---|---|
| `basement >= 0.0.16` | removed |
| `foundation` | removed |
| `memory >= 0.18` | removed |
| `cardano-crypto >= 1.2 && < 1.4` | removed |
| `crypton >= 0.32 && < 1.1` | `crypton >= 1.1 && < 1.2` |
| — | `ram >= 0.22 && < 0.23` (new) |
