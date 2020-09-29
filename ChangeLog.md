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
