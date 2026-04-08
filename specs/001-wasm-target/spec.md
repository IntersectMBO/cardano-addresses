# Feature Specification: WASM Build Target

**Feature Branch**: `001-wasm-target`
**Created**: 2026-04-08
**Status**: Draft
**Input**: User description: "WASM target: eliminate basement/foundation to enable wasm32-wasi-ghc build"

## User Scenarios & Testing

### User Story 1 - Library consumer builds cardano-addresses for WASM (Priority: P1)

A developer building a browser-based or serverless Cardano application wants to use cardano-addresses compiled to WebAssembly. They run the WASM cross-compiler against the library and get a working `.wasm` artifact that can generate, parse, and inspect Cardano addresses.

**Why this priority**: This is the core deliverable. Without the library compiling, nothing else matters.

**Independent Test**: Build the library with `wasm32-wasi-ghc` using the provided `cabal-wasm.project`. All modules compile without errors.

**Acceptance Scenarios**:

1. **Given** the cardano-addresses source and GHC WASM toolchain, **When** a developer runs `wasm32-wasi-cabal --project-file=cabal-wasm.project build cardano-addresses`, **Then** the library compiles successfully for the `wasm32-wasi` target.
2. **Given** a compiled WASM library, **When** a developer links it into a test program that generates a Shelley address from a mnemonic, **Then** the program runs correctly under `wasmtime` and produces a valid bech32 address.

---

### User Story 2 - Existing native build remains unchanged (Priority: P1)

Library maintainers and existing users must not be affected. The native GHC build, test suite, and CI must continue working exactly as before.

**Why this priority**: Regression protection is non-negotiable for an upstream-targeted change.

**Independent Test**: Run `cabal build` and `cabal test unit` with the standard project file. All 720 tests pass.

**Acceptance Scenarios**:

1. **Given** the modified source, **When** building with the native GHC toolchain using the existing `cabal.project`, **Then** the library compiles and all existing tests pass.
2. **Given** the modified source, **When** CI runs on the standard matrix (GHC 9.6/9.10/9.12, Linux/macOS/Windows), **Then** all builds succeed and all tests pass.

---

### User Story 3 - Address round-trip works on WASM (Priority: P2)

A developer wants confidence that core address operations produce identical results on WASM and native targets. Key operations (mnemonic to root key, key derivation, address generation, address inspection) must be byte-identical across targets.

**Why this priority**: Correctness is essential for a cryptographic library, but can be validated after the build works.

**Independent Test**: Run a test program on both native and WASM that generates addresses from known test vectors and compares output.

**Acceptance Scenarios**:

1. **Given** a known 15-word mnemonic and derivation path, **When** generating a Shelley payment address on WASM, **Then** the output matches the native-compiled result byte-for-byte.
2. **Given** a known Byron address, **When** inspecting it on WASM, **Then** the JSON output matches the native-compiled result.

---

### Edge Cases

- What happens when the WASM build encounters a C dependency that requires platform-specific assembly (e.g. AES-NI)? The build must fall back to portable C implementations.
- What happens when `Crypto.Random.Entropy.getEntropy` is called on WASM? WASI provides `/dev/urandom`, so this should work, but must be verified.
- What happens with Template Haskell dependencies? The `bech32-th` package uses TH; this must be handled by GHC's external interpreter on WASM, or the dependency can be made conditional.

## Requirements

### Functional Requirements

- **FR-001**: Library MUST compile with `wasm32-wasi-ghc` (GHC 9.12+) to the `wasm32-wasi` target.
- **FR-002**: Library MUST NOT depend on `basement`, `foundation`, or `memory` packages.
- **FR-003**: Library MUST use `ram` (the `memory` replacement) for byte array operations.
- **FR-004**: Library MUST use `crypton` >= 1.1 (which depends on `ram` instead of `memory`).
- **FR-005**: Library MUST vendor the `Cardano.Crypto.Wallet` functionality currently provided by `cardano-crypto`.
- **FR-006**: Library MUST vendor the BIP39 mnemonic functionality currently provided by `cardano-crypto`.
- **FR-007**: Library MUST continue to build and pass all tests with the native GHC toolchain.
- **FR-008**: Library MUST provide a `cabal-wasm.project` file with correct WASM build configuration.
- **FR-009**: All cryptographic operations MUST produce identical results on WASM and native targets.

### Key Entities

- **Vendored Wallet module**: The `Cardano.Crypto.Wallet` cluster (~565 lines) providing XPrv/XPub key operations, copied from `cardano-crypto` with `basement` imports replaced.
- **Vendored BIP39 module**: The `Crypto.Encoding.BIP39` cluster (~600 lines + 2081-line word list) providing mnemonic encoding/decoding, with `basement` types replaced by standard Haskell types.
- **WASM project file**: `cabal-wasm.project` configuring mmap emulation for `ram` and thread disabling for `crypton`.

## Success Criteria

### Measurable Outcomes

- **SC-001**: `wasm32-wasi-cabal build cardano-addresses` succeeds, producing a WASM library artifact.
- **SC-002**: All 720 existing unit tests pass on native builds after the dependency changes.
- **SC-003**: Known test vectors (mnemonic -> address) produce identical output on native and WASM targets.
- **SC-004**: The `basement`, `foundation`, `cardano-crypto`, and `memory` packages are absent from the dependency closure.

## Assumptions

- GHC WASM toolchain is obtained via `ghc-wasm-meta` nix flake (GHC 9.12+).
- WASI mmap emulation (`-D_WASI_EMULATED_MMAN`) is acceptable for `ram`'s secure memory operations.
- Single-threaded execution is acceptable on WASM (no `pthread` support needed).
- The `cardano-crypto` C sources (ed25519-donna, encrypted signing) compile with `wasi-sdk` / `wasm32-wasi-clang`.
- Template Haskell in `bech32-th` works via GHC's external interpreter on WASM, or the dependency can be made conditional.
