## [4.0.3]

### Added

- Automated draft release preparation by creating fixed part and extracting the latest entries from `Changelog.md`	
- Fix `spanish` last word in dictionary
- Fixed cross-compilation (from Linux to Windows) by replacing runtime `git` call in `System.Git.TH` with CPP macro (ie., `GITREV`) defaulting to "unknown" but allowing overriding via `-DGITREV`<<<<<<< HEAD
- Fix `nix develop` to work smoothly, adding darwin to packages when `nix build`, adding `nix-build` for docker plus docs how to do everything
- Integrate `Docusaurus` with README

## [4.0.2] - 2025-12-30

### Added

- Fixed `keyHashFromText`

## [4.0.1] - 2025-10-30

### Added

- Support for other languages, i.e., `spanish`, `italian`, `french`, `korean` and `japanese` in mnemonic generation
- Extend upper bound `optparse-applicative` to `< 0.20`

## [4.0.0] - 2025-02-04

### Added

- Unify core and command-line packages into one
- Drop JavaScript support and get rid of JavaScript codebase
- CIP-0129 support added, affecting `cardano-address key hash` and `cardano-address script hash`
- CIP-0129 support with backward compatibility added, affecting `cardano-address key hash` and `cardano-address script hash`
- `scriptHashToText`, `scriptHashFromText` and `prettyErrScriptHashFromText` added to `Cardano.Address.Script`
- Simplify filenames by using a hashed prefix instead of the full mnemonic, enhancing portability).
- `hashKey` for Shelley style
- Key derivation support for DRep, CCCold and CCHot in accordance with [CIP-0105](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0105). Also supported in CLI.
- Fix field for staking scripts in AddressInfo.
- Rename infoScriptHash field in AddressInfo to infoSpendingScriptHash.
- Get signing key and chain code from extended signing key via `cardano-address key private`
- Support for new environments `preview` and `preprod`.
- Supported derivation of root private key using second factor mnemonic or its base16/base64/plain text equivalent

## [3.12.0] - 2022-08-17

### Added

- Supported credential construction from key hashes in both payment and delegation credential. The construction scheme is also supported in CLI.

## [3.11.0] - 2022-06-21

### Added

- Compatibility with Aeson 2 library.

## [3.10.0] - 2022-05-26

### Added

- `cardano-address key walletid` support for Shared wallets.

## [3.9.0] - 2022-03-08

### Added

- Support for policy key derivation and hashing according to [CIP-1855](https://github.com/cardano-foundation/CIPs/tree/master/CIP-1855).

## [3.8.0] - 2022-02-11

### Added

- Adds the `cardano-address key walletid` command, which derives a
  [`cardano-wallet` wallet ID](https://input-output-hk.github.io/adrestia/cardano-wallet/design/WalletId)
  from a wallet key.

### Changed

- Several nodejs package dependencies updated according to `npm audit` and dependabot alerts.


## [3.7.0] - 2021-12-15

### Added

- Address type in address inspect command.


## [3.6.0] - 2021-09-08

### Changed

- Bech32 prefixes for shared account addresses were fixed to match the
  latest draft of [CIP-1854][].

[CIP-1854]: https://github.com/cardano-foundation/CIPs/blob/master/CIP-1854/CIP-1854.md


## [3.5.0] - 2021-06-28

### Added

- Initial release of a [NPM Package](https://www.npmjs.com/package/cardano-addresses)
  which uses the GHCJS build.

### Changed

- The supported compiler version is now GHC 8.10.4.

- Updated `validateScriptTemplate` and `validateScriptOfTemplate`
  functions for multi-signature wallets.


## [3.4.0] - 2021-04-30

### Added

- Added `Cardano.Address.Style.Shelley.eitherInspectAddress` function
  with stronger result and error types.

- Added `Cardano.Address.Style.Shared` module which defines a shared
  wallet style enabling multisig.

### Changed

- The constructors of `Cardano.Address.Style.Shelley.ErrInspectAddress`
  have changed.
  Any code which pattern matches on this type will need minor changes.

- A number of Bech32 prefixes were changed to account for CIP changes.
  The whole family of `*_shared_*` prefixes were introduced to accommodate
  newly added shared wallet style. In specific, there in no longer `script_vkh`
  but `addr_shared_vkh` and `stake_shared_vkh` to denote spending and stake
  verification key hashes, respectively.

- `KeyHash` now needs `KeyRole` values to be specified, except binary
  payload. It was needed change to enable differentiating between
  spending and stake key hashes.

### Removed

- Multisig related functions were deleted from `Cardano.Address.Style.Shelley` as they
  found a new place in `Cardano.Address.Style.Shared`.


## [3.3.0] - 2021-04-09

### Added

- The library now builds with ghcjs and passes tests running on nodejs.

- The `cardano-address inspect` command now includes bech32
  encodings. In the output, the JSON attribute will have a `_bech32`
  suffix. For example, `stake_key_hash_bech32`.

### Changed

- The `Index` type no longer has an `Enum` instance. Use
  `indexFromWord32` and `indexToWord32` instead.

- (Breaking change) The `Cardano.Address.bech32` function will now use
  the `addr_test` prefix instead of `addr`, if the given address is
  not a mainnet address.

### Removed

N/A


## [3.2.0] - 2020-01-22

### Added

- Added definition for 'Cosigner'.

- Added definition for 'ScriptTemplate', `ToJSON` and `FromJSON` instances added.

- Added validation of 'ScriptTemplate'.

- Added command-line for 'cardano-address script validate'

- Introduced 'foldScript' function.

### Changed

- Fixed `cardano-address --version` reporting.

- Added Ord instance for 'ScriptHash'.

- Extended script to account for ActiveFromSlot and ActiveUntilSlot, ie., introducing timelocks. `ToJSON` and `FromJSON` instances were updated. Also command-line supports
  timelocks now via 'active_from' and 'active_until`.

- Introduced Required and Recommended validation and adjusted the current one.

- Fixed cardano-address address bootstrap example in command-line help.

- Add missing crc32 integrity check when decoding Byron/Icarus addresses.

- Parametrized 'Script', and use it with 'ScriptHash' and 'Cosigner'.

- Better error reporting upon validation.

- Correcting cardano-address hash example in command-line help.

- Adding more examples to README.md

### Removed

N/A

## [3.1.0] - 2020-11-13

### Added

N/A

### Changed

- 'keyHashFromText' now works seamlessly with key, extended keys or key hashes. In case a key or extended key is given, the relevant part will be hashed on the fly. Said differently, it means that
  the command-line and the JSON instance for 'Script' works transparently with keys or key hashes.

- Fixed a bug with the `key hash` command which failed when provided with extended keys.

- The 'FromJSON' instance for 'Script' now runs the validation within the JSON parser, such that when the parser succeeds the resulting 'Script' is indeed valid.

- The 'FromJSON' instance for 'Script' is now much better at showing errors.

### Removed

N/A

## [3.0.0] - 2020-11-12

### Added

- Support for (multisig) scripts and script addresses in modules:
  - `Cardano.Address.Script`
  - `Cardano.Address.Script.Parser`

- Support for constructing scripts and script addresses via the command-line.

- Support for constructing rewards addresses via the library and command-line.

- New command for computing key and script hashes that are required in the construction of larger objects (e.g. addresses).

- Support for cabal build.

### Changed

- The command-line API no longer support multi-encoding (base16, bech32 and base58) but instead, enforces bech32 for keys and addresses, with specific human readable prefixes. It is still possible to easily go from base16-encoded data to bech32 by piping data through the [`bech32`](https://github.com/input-output-hk/bech32/) command-line.

- It is no longer possible to derive child keys to and from any path. Are only allowed:
   - root -> account
   - root -> address
   - account -> address
  This is reflected in the bech32 prefixes of the inputs and outputs.

- Allow constructing delegation addresses from a script. This works transparently from previous version of the command-line, but the command now also accepts script hashes as possible valid inputs.

### Removed

- No more `--legacy` option on the `key child` command. Which derivation scheme to use is now inferred from the bech32 prefixe used and the derivation path.

- `Cardano.Address.Errors` module. Errors data-types have been moved to their respective module `Cardano.Address.Styles.{Byron,Icarus,Shelley}`

- Anything related to Jormungandr in both the library and the command-line.

## [2.1.0] - 2020-09-29

### Added

- Added constructors to derive keys on the multisig role.
- Made the parser for `--network-tag` more user friendly by now accepting pre-defined keywords such as "mainnet" or "testnet".

### Changed

- Renamed `AccountingStyle` into `Role` to better capture the semantic of the 4th level in derivation paths.
- Made script hashes 28-byte long again, after this was fixed upstream in the Cardano ledger.

### Removed

N/A


## [2.0.0] - 2020-09-10

### Added

- Command-line interface `cardano-address` for managing recovery-phrases, keys and addresses.
- Support for Shelley-specific address types.
- Support for Jormungandr-specific address types.

### Changed

- Repository structure re-organized in two packages: core & command-line.

### Removed

N/A


## [1.0.0] - 2020-04-21

### Added

- 'Cardano.Mnemonic' module for mnemonic generation and manipulation.
- 'Cardano.Address' module for address creation, encoding and decoding.
- 'Cardano.Address.Derivation' module for primitives and abstractions regarding hierarchical derivation of credentials.
- 'Cardano.Address.Style.Byron' module implementing derivation primitives for 'Byron' addresses.
- 'Cardano.Address.Style.Icarus' module implementing derivation primitives for 'Icarus' addresses.

### Changed

N/A

### Removed

N/A
