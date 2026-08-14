---
sidebar_position: 4
title: Ownership proving using ZKP (experimental)
---

:::warning Experimental

This is an **experimental** feature. The circuits, CLIs, and workflows described
below are developed in the
[`cardano-foundation/bls`](https://github.com/cardano-foundation/bls)
repository and are **not** part of the `cardano-addresses` release artifacts.
The ceremonies used here are single-party dev ceremonies — do **not** use them
in production without a proper multi-party trusted setup. Expect breaking
changes.

:::

## Overview

`cardano-addresses` derives the keys; it does not itself generate ZK proofs.
Proving ownership of a Cardano key with a zero-knowledge proof is provided by the
[`cardano-foundation/bls`](https://github.com/cardano-foundation/bls) project,
which hosts the Circom circuits and the Rust CLIs that drive them. The goal is to
prove knowledge of the private Ed25519 scalar `sk` such that the public key
`A = [sk]·G` matches a given compressed key — without ever revealing `sk`. A real
Cardano payment key is derived with `cardano-address` (CIP-1852 path
`1852H/1815H/0H/0/0`); in BIP32-Ed25519 the first 32 bytes of the extended
signing key (`kL`) already hold the clamped scalar the circuit needs as its
private witness. The circuit is compiled once to `.r1cs` + `.wasm` with `circom`
on the BLS12-381 field and reused for any key — only the per-user `input.json`
changes. `snarkjs` turns `input.json` + `.wasm` into a witness, the
[`trusted-setup` CLI](https://github.com/cardano-foundation/bls/tree/main/clis/trusted-setup)
runs a single-party Groth16 ceremony, and the
[`groth16` CLI](https://github.com/cardano-foundation/bls/tree/main/clis/groth16)
proves and verifies. Two circuit families exist:
`CardanoKeyOwnership` proves ownership of a single public key, while
`CardanoKeyOwnershipSMT` additionally proves that the key is a member of an
authorized key set committed into a Sparse Merkle Tree — so the verifier only
trusts and stores the single Merkle root instead of an `O(N)` key list. Both come
in a monolithic (~1.97M constraints) and a Nova step-chain variant that splits the
scalar multiplication into 255 × 7.7K-constraint steps, cutting the ceremony from
~8 minutes to ~3 seconds and the proving key from 1.2 GB to 5 MB. The pipeline is
driven by three Rust CLIs in `clis/` — `trusted-setup`, `smt`, and `nova` — which
are described below.

The reference end-to-end flow is documented upstream at
[`circom/CardanoKeyOwnership`](https://github.com/cardano-foundation/bls/tree/main/circom/CardanoKeyOwnership#end-to-end-flow--implementation-8-nova-step-chain)
and [`circom/CardanoKeyOwnershipSMT`](https://github.com/cardano-foundation/bls/tree/main/circom/CardanoKeyOwnershipSMT).

## The CLIs

All CLIs are Rust binaries in the
[`cardano-foundation/bls/clis`](https://github.com/cardano-foundation/bls/tree/main/clis)
directory:

| CLI | Build | Purpose |
|-----|-------|---------|
| [`trusted-setup`](https://github.com/cardano-foundation/bls/tree/main/clis/trusted-setup) | `cd clis/trusted-setup && cargo build --release` | Groth16 trusted-setup ceremonies on BLS12-381: `ceremony-dev` (single-party dev ceremony, `--sparse` / `--h-scalar`) and `phase2` (multi-party MPC on a public `.ptau` SRS) |
| [`groth16`](https://github.com/cardano-foundation/bls/tree/main/clis/groth16) | `cd clis/groth16 && cargo build --release` | Proof generation (`prove`), verification (`verify`), and verifying-key export (`export-vk`) |
| [`smt`](https://github.com/cardano-foundation/bls/tree/main/clis/smt) | `cd clis/smt && cargo build --release` | Sparse Merkle Tree operations for `CardanoKeyOwnershipSMT`: `smt key` (Ed25519 decompression + limb chunking + MiMC leaf), `smt leaf`, `smt insert`, `smt digest`, `smt path`, `smt verify`, and `smt cardano-input` (full circuit-input assembly) |
| [`nova`](https://github.com/cardano-foundation/bls/tree/main/clis/nova) | `cd clis/nova && cargo build --release` | Nova IVC step-chain flow: `nova params`, `nova ceremony`, `nova fold`, `nova compress`, `nova verify` |

## CardanoKeyOwnership — proving ownership of a single key

The circuit proves `A = PointCompress([sk]·G)` on Curve25519, where `sk` is the
clamped scalar from a real Cardano payment key and `A` its compressed public key.

```bash
BLS=<path-to-bls-repo>
cd $BLS/circom/CardanoKeyOwnership

# 1. Derive a real Cardano payment key
cardano-address recovery-phrase generate --size 15 > phrase.prv
cardano-address key from-recovery-phrase Shelley < phrase.prv > root.xsk
cardano-address key child 1852H/1815H/0H/0/0 < root.xsk > pay.xsk
cardano-address key public --without-chain-code < pay.xsk > pay.vk

# 2. Build the witness input (A, sk, PointA) from the bech32 keys
python3 gen_cardano_address_input.py --xsk pay.xsk --vk pay.vk -o input.json

# 3. Compile the circuit (once, reused for any key)
circom --prime bls12381 -l ../Ed25519Verify/node_modules/circomlib/circuits \
  cardano_ed25519_ownership.circom --r1cs --wasm --sym

# 4. Generate the witness
snarkjs wtns calculate \
  cardano_ed25519_ownership_js/cardano_ed25519_ownership.wasm \
  input.json witness_ownership.wtns

# 5. Single-party dev ceremony (once per circuit, ~8 min; --sparse --h-scalar required)
cd $BLS/clis/trusted-setup && cargo build --release
target/release/trusted-setup ceremony-dev --sparse --h-scalar \
  --circuit $BLS/circom/CardanoKeyOwnership/cardano_ed25519_ownership.r1cs \
  --proving-key cko.pk --verifying-key cko.vk

# 6. Prove
cd $BLS/clis/groth16
cargo run --release -- prove --sparse \
  --circuit $BLS/circom/CardanoKeyOwnership/cardano_ed25519_ownership.r1cs \
  --witness $BLS/circom/CardanoKeyOwnership/witness_ownership.wtns \
  --proving-key cko.pk --out proof.bin

# 7. Verify
cargo run --release -- verify \
  --proof proof.bin --public proof.pub --verifying-key cko.vk
# → Verification result: VALID
```

### Nova step-chain variant (recommended for Ed25519)

The same statement can be split into 255 identical 7,724-constraint steps and
folded with Nova (Implementation 8). The ceremony drops to ~3 s and the proving
key to 5 MB — at the cost of `O(N)` verification and a sequential step chain.

```bash
cd $BLS/clis/nova && cargo build --release

cd $BLS/circom/CardanoKeyOwnership
circom --prime bls12381 -l ../Ed25519Verify/node_modules/circomlib/circuits \
  cardano_ed25519_ownership_nova.circom --r1cs --wasm --sym

# Inspect the step circuit (must report n_pub_in == n_pub_out == 24)
$BLS/clis/nova/target/release/nova params --circuit cardano_ed25519_ownership_nova.r1cs

# One ceremony for the step circuit (seconds, reusable)
$BLS/clis/nova/target/release/nova ceremony \
  --circuit cardano_ed25519_ownership_nova.r1cs \
  --proving-key cko255.pk --verifying-key cko255.vk

# Generate 255 step witnesses step_0000.wtns … step_0254.wtns iteratively
# (dblIn := extended(G), addIn := extended(O), sel := (sk >> i) & 1, LSB-first)
# via: snarkjs wtns calculate cardano_ed25519_ownership_nova_js/cardano_ed25519_ownership_nova.wasm

# Fold — prove each step, check the state chain, accumulate the transcript
$BLS/clis/nova/target/release/nova fold \
  --circuit cardano_ed25519_ownership_nova.r1cs \
  --proving-key cko255.pk --steps <witness-dir> --out cko255_ivc.json

# Verify — re-checks every pairing, the state chain, and the transcript
$BLS/clis/nova/target/release/nova verify --ivc cko255_ivc.json --verifying-key cko255.vk
# → Verified 255 steps: 255 pairings OK, state chain OK, transcript OK
```

## CardanoKeyOwnershipSMT — set-based authorization with a Merkle root

The SMT variant combines Ed25519 key ownership with Sparse Merkle Tree membership:
authorized public keys are committed (as one-way MiMC leaves, never raw keys) into
an SMT, and a single proof shows that the prover owns `A` **and** that `A` is
authorized by the tree root. The verifier stores only the root, so the
authorized-set state stays constant regardless of `N`. Key rotation and revocation
are root updates.

```bash
cd $BLS/clis/smt && cargo build --release

cd $BLS/circom/CardanoKeyOwnershipSMT

# Derive a real Cardano payment key (as above)
cardano-address recovery-phrase generate --size 15 > phrase.prv
cardano-address key from-recovery-phrase Shelley < phrase.prv > root.xsk
cardano-address key child 1852H/1815H/0H/0/0 < root.xsk > pay.xsk
cardano-address key public --without-chain-code < pay.xsk > pay.vk

# Build the full circuit input (A, sk, PointA, smt_root, siblings, directions);
# all crypto is done by the smt CLI, not by Python
./gen_input.sh --xsk pay.xsk --vk pay.vk --depth 4 --output input.json \
  --smt-cli $BLS/clis/smt/target/release/smt

# Compile, witness, ceremony, prove, verify — same shape as CardanoKeyOwnership,
# with cardano_key_ownership_smt.circom
circom --prime bls12381 -l ../Ed25519Verify/node_modules/circomlib/circuits \
  cardano_key_ownership_smt.circom --r1cs --wasm --sym
snarkjs wtns calculate cardano_key_ownership_smt_js/cardano_key_ownership_smt.wasm \
  input.json witness.wtns

$BLS/clis/trusted-setup/target/release/trusted-setup ceremony-dev --sparse --h-scalar \
  --circuit cardano_key_ownership_smt.r1cs --proving-key smt.pk --verifying-key smt.vk

cd $BLS/clis/groth16 && cargo build --release
target/release/groth16 prove --sparse \
  --circuit $BLS/circom/CardanoKeyOwnershipSMT/cardano_key_ownership_smt.r1cs \
  --witness $BLS/circom/CardanoKeyOwnershipSMT/witness.wtns \
  --proving-key smt.pk --out proof.bin
target/release/groth16 verify --proof proof.bin --public proof.pub --verifying-key smt.vk
# → Verification result: VALID
```

The SMT step-chain (Nova) variant `cardano_key_ownership_smt_nova.circom` folds
the scalar multiplication into 255 steps exactly as in the `CardanoKeyOwnership`
Nova flow; the SMT-membership half stays in the monolithic circuit.

## CardanoKeyOwnership vs CardanoKeyOwnershipSMT

| Feature | CardanoKeyOwnership | CardanoKeyOwnershipSMT |
|---------|---------------------|------------------------|
| Proves key ownership | ✓ | ✓ |
| Proves set membership | ✗ | ✓ |
| Verifier trust / state | Per public key `A` | Single SMT root |
| Authorized set size | 1 | Any `N` (constant verification state) |
| Public inputs | `A[256]` | `A[256]`, `smt_root` |
| Hides which key | ✗ (`A` public) | ✗ (`A` public) — hides path/index only |
| Circuit size (monolithic) | 1,967,405 constraints | 1,971,079 constraints (+0.2 % at depth 4) |
| Circuit size (Nova) | 255 × 7,724 constraints | 255 × 7,724 constraints (SMT part stays monolithic) |
| Set rotation / revocation | n/a (per-key proof) | Root update (rebuild SMT) |
| Needs a key registry | ✗ | ✓ (root + Merkle path) |
| SMT CLI integration | ✗ | ✓ |

In short: use **CardanoKeyOwnership** to prove "I own this specific key `A`", and
**CardanoKeyOwnershipSMT** to prove "I own a key that is authorized in this set"
— the set being committed by a single Merkle root that the verifier must trust.

## Implementation trade-offs: monolithic Groth16 vs Nova step-chain

Independently of the circuit family, the same statement can be proven as a single
monolithic Groth16 proof (Implementation 7) or decomposed into 255 small steps
and folded with Nova IVC (Implementation 8). The two paths have very different
trade-offs, measured on the same machine with the same key:

| Phase | Monolithic Groth16 | Nova step-chain |
|---|---|---|
| Circuit | 1,967,405 constraints | 255 × 7,724 constraints |
| Witness generation | ~10 s | 255 steps: ~133 s (sequential) |
| Ceremony (one-time, reusable) | ~8 min | ~3 s |
| Prove / fold | ~74 s | ~179 s |
| Verify | ~1.5 s (one pairing) | ~3.2 s (255 pairings, `O(N)`) |
| **e2e first run (incl. ceremony)** | **~9.7 min** | **~5.2 min** |
| **e2e steady state (ceremony amortized)** | **~86 s** | **~312 s** |
| Proving key | 1.2 GB | 5 MB |
| Verifying key | 178 MB | 719 KB |
| Peak memory | ~4.5 GiB | per-step |

Takeaways:

- **First run — Nova wins.** The monolithic ceremony dominates (~8 min) and needs
  ~4.5 GiB of RAM, while the Nova ceremony is ~3 s and the fold uses per-step
  memory. The proving key shrinks from 1.2 GB to 5 MB.
- **Steady state — monolithic Groth16 wins (~3.6×).** Once the ceremony is
  amortized, a new key costs ~86 s (one witness + one proof) vs ~312 s (255
  step witnesses + fold). The step chain is inherently sequential — each step
  feeds the next — so it cannot be parallelized.
- **Verification.** The monolithic proof verifies with a single Groth16 pairing in
  ~1.5 s and is a standalone, constant-size artifact — the natural fit for
  on-chain verification. Nova verification is `O(N)`: it re-checks all 255
  pairings plus the state chain and the transcript (~3.2 s), and the bundle grows
  linearly with the number of steps.
- **Deployment footprint.** Nova's ceremony is trivial but verification is not
  constant-time; Groth16's ceremony is heavy but is run once per circuit and then
  amortized across unlimited keys.
- **Both prove the same statement.** The point-compression and `addOut == 2·PointA`
  checks are done by the application outside the Nova fold; a single monolithic
  proof encodes them in-circuit.

Rule of thumb: **Nova** if you prove a fresh key once (or run on constrained
hardware), **monolithic Groth16** if you prove many keys against a pre-computed
ceremony or need constant-size, cheap on-chain verification.

## Further reading

- [`CardanoKeyOwnership` reference](https://github.com/cardano-foundation/bls/tree/main/circom/CardanoKeyOwnership) — full end-to-end flows, Implementation 7 (monolithic) and Implementation 8 (Nova step-chain), plus benchmarks
- [`CardanoKeyOwnershipSMT` reference](https://github.com/cardano-foundation/bls/tree/main/circom/CardanoKeyOwnershipSMT) — SMT design, security considerations, and benchmarks
- [`trusted-setup` CLI](https://github.com/cardano-foundation/bls/tree/main/clis/trusted-setup) — ceremony commands (`ceremony-dev`, `phase2`)
- [`smt` CLI](https://github.com/cardano-foundation/bls/tree/main/clis/smt) — SMT operations and circuit-input generation
- [`nova` CLI](https://github.com/cardano-foundation/bls/tree/main/clis/nova) — Nova IVC step-chain and NIFS compression flow
