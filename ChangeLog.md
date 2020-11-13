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
