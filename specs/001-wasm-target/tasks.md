# Tasks: WASM Build Target

**Input**: Design documents from `/specs/001-wasm-target/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2)
- Include exact file paths in descriptions

---

## Phase 1: Setup

**Purpose**: Dependency and build configuration changes

- [ ] T001 Add `ram` dependency and bump `crypton` bound to `>= 1.1 && < 1.2` in `cardano-addresses.cabal`
- [ ] T002 Create `cabal-wasm.project` with WASM-specific flags (mmap emulation, argon2 no-threads, shared: False) at repo root

---

## Phase 2: Foundational (Vendor Modules)

**Purpose**: Vendor `cardano-crypto` modules into the project, eliminating `basement`/`foundation`/`memory` dependencies

**CRITICAL**: All user story work depends on this phase completing. The library must continue to build natively after each task.

- [ ] T003 [P] Vendor `Cardano.Crypto.Wallet.Types` (~29 lines) into `lib/Cardano/Address/Crypto/Wallet/Types.hs` — replace `Foundation`/`Foundation.Check`/`Foundation.Collection` imports with `base`/`QuickCheck`; drop or rewrite `Arbitrary` instance
- [ ] T004 [P] Vendor `Cardano.Crypto.Wallet.Encrypted` (~226 lines) into `lib/Cardano/Address/Crypto/Wallet/Encrypted.hs` — replace `Data.ByteArray` import source from `memory` to `ram`; copy C sources (`cbits/encrypted_sign.c`, `cbits/ed25519/`) into project `cbits/`
- [ ] T005 [P] Vendor `Cardano.Crypto.Wallet.Pure` (~108 lines) into `lib/Cardano/Address/Crypto/Wallet/Pure.hs` — replace `Data.ByteArray` import source from `memory` to `ram`
- [ ] T006 Vendor `Cardano.Crypto.Wallet` (~202 lines) into `lib/Cardano/Address/Crypto/Wallet.hs` — replace `Basement.Compat.Typeable` with `Data.Typeable`; depends on T003-T005
- [ ] T007 [P] Vendor `ListN` (~80 lines) from `basement` into `lib/Cardano/Address/Crypto/ListN.hs` — extract `ListN` newtype, `toListN`, `unListN`, `map`, `mapM`, `foldl'` using `GHC.TypeLits` instead of `Basement.Nat`
- [ ] T008 [P] Vendor `Crypto.Encoding.BIP39.Dictionary` (~88 lines) into `lib/Cardano/Address/Crypto/BIP39/Dictionary.hs` — replace `Offset String` with `Word16` newtype for `WordIndex`; replace `NormalForm` with `NFData`; replace `TryFrom` with `Maybe`-returning function; replace `Basement.Imports` with `Prelude`
- [ ] T009 [P] Vendor `Crypto.Encoding.BIP39.English` (~2081 lines) into `lib/Cardano/Address/Crypto/BIP39/English.hs` — replace `Vect 2048 String` with `Vector Text`; replace `Basement.Imports` with `Prelude`
- [ ] T010 Vendor `Crypto.Encoding.BIP39` (~440 lines) into `lib/Cardano/Address/Crypto/BIP39.hs` — replace `Basement.Sized.List` with vendored `ListN`; replace `Basement.String` with `Text`; replace `Basement.Nat` with `GHC.TypeLits`; replace `NormalForm` with `NFData`; depends on T007-T009
- [ ] T011 Update `Cardano.Address.Crypto` facade in `lib/Cardano/Address/Crypto.hs` — re-export from vendored wallet modules instead of `cardano-crypto`; re-export `Data.ByteArray` from `ram` instead of `memory`
- [ ] T012 Update `Cardano.Mnemonic` in `lib/Cardano/Mnemonic.hs` — import from vendored `Cardano.Address.Crypto.BIP39` instead of `Crypto.Encoding.BIP39`; replace `Basement.NormalForm` with `Control.DeepSeq`; replace `Basement.Sized.List` with vendored `ListN`; replace `Basement.String` with `Text`
- [ ] T013 Remove `cardano-crypto`, `basement`, `foundation`, `memory` from `build-depends` in `cardano-addresses.cabal`; add vendored modules to `exposed-modules` and `other-modules`; add `cbits/` C sources
- [ ] T014 Update `.hlint.yaml` — replace `Cardano.Crypto.Wallet` allowed modules with vendored module paths
- [ ] T015 Verify native build: run `cabal build cardano-addresses && cabal test unit` — all 720 tests must pass

**Checkpoint**: Library builds natively with zero `basement`/`foundation`/`memory`/`cardano-crypto` dependencies. All existing tests pass.

---

## Phase 3: User Story 1 - WASM Build (Priority: P1) MVP

**Goal**: Library compiles with `wasm32-wasi-ghc`

**Independent Test**: `wasm32-wasi-cabal --project-file=cabal-wasm.project build cardano-addresses` succeeds

- [ ] T016 [US1] Attempt WASM build with `wasm32-wasi-cabal --project-file=cabal-wasm.project build cardano-addresses` and capture errors
- [ ] T017 [US1] Fix vendored C sources for wasi-sdk compilation — ensure `cbits/ed25519/` and `cbits/encrypted_sign.c` compile with `wasm32-wasi-clang` (may need `#ifdef __wasm__` guards for architecture detection)
- [ ] T018 [US1] Handle Template Haskell — if `bech32-th` fails, add conditional `if !arch(wasm32)` in cabal file or replace TH usage with runtime validation
- [ ] T019 [US1] Fix any remaining WASM compilation errors (iterative — dependency issues, missing WASI APIs, etc.)
- [ ] T020 [US1] Verify full WASM library build succeeds end-to-end

**Checkpoint**: `wasm32-wasi-cabal build cardano-addresses` completes successfully.

---

## Phase 4: User Story 2 - Native Regression (Priority: P1)

**Goal**: Existing native build and test suite remain green

**Independent Test**: `cabal build && cabal test unit` passes on all CI platforms

- [ ] T021 [US2] Run `cabal test unit` on native GHC 9.6 — all 720 tests pass
- [ ] T022 [US2] Run stylish-haskell on all new/modified `.hs` files
- [ ] T023 [US2] Run hlint on all new/modified `.hs` files — zero hints
- [ ] T024 [US2] Verify `cabal build` succeeds with existing `cabal.project` (no WASM flags)

**Checkpoint**: Native build and all tests pass with code quality checks green.

---

## Phase 5: User Story 3 - WASM Correctness (Priority: P2)

**Goal**: Address operations produce identical results on WASM and native

**Independent Test**: Test program generates addresses from known mnemonics, output matches between targets

- [ ] T025 [US3] Create a small test program in `wasm/test/Main.hs` that generates Shelley and Byron addresses from known test mnemonics and prints them
- [ ] T026 [US3] Build and run test program natively, capture output as golden reference
- [ ] T027 [US3] Build test program for WASM, run under `wasmtime`, compare output to golden reference
- [ ] T028 [US3] Document any WASM-specific limitations in `wasm/README.md`

**Checkpoint**: Address generation is byte-identical across native and WASM targets.

---

## Phase 6: Polish

**Purpose**: Documentation, cleanup, CI

- [ ] T029 [P] Update `cardano-addresses.cabal` version and changelog
- [ ] T030 [P] Add WASM build instructions to project README or `wasm/README.md`
- [ ] T031 Run `cabal check` to ensure Hackage readiness
- [ ] T032 Final review: verify `basement`, `foundation`, `memory`, `cardano-crypto` are absent from `cabal build --dry-run` output

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — start immediately
- **Foundational (Phase 2)**: Depends on Phase 1 — BLOCKS all user stories
- **US1 WASM Build (Phase 3)**: Depends on Phase 2
- **US2 Native Regression (Phase 4)**: Depends on Phase 2 — can run in parallel with Phase 3
- **US3 WASM Correctness (Phase 5)**: Depends on Phase 3 (needs WASM build working)
- **Polish (Phase 6)**: Depends on Phases 3 and 4

### Within Phase 2 (Foundational)

```
T003, T004, T005 ─── parallel ───→ T006
T007, T008, T009 ─── parallel ───→ T010
                                    T006, T010 → T011 → T012 → T013 → T014 → T015
```

### Parallel Opportunities

- T003/T004/T005 (wallet submodules) can run in parallel
- T007/T008/T009 (BIP39 submodules + ListN) can run in parallel
- Phase 3 and Phase 4 can run in parallel after Phase 2

---

## Implementation Strategy

### MVP First (Phases 1-3)

1. Complete Phase 1: Setup (cabal changes)
2. Complete Phase 2: Vendor all modules, verify native build
3. Complete Phase 3: WASM build succeeds
4. **STOP and VALIDATE**: Library compiles for WASM

### Incremental Delivery

1. Setup + Foundational → native build green, deps eliminated
2. Add WASM build → library compiles to `.wasm`
3. Add correctness tests → byte-identical output verified
4. Polish → documentation, CI, Hackage readiness

---

## Notes

- Commit after each task or logical group
- Run `cabal build` after each Phase 2 task to catch breakage early
- The BIP39 vendoring (T007-T010) is the most complex work — budget accordingly
- C source vendoring (T004) includes copying the entire `cbits/ed25519/` directory from cardano-crypto
