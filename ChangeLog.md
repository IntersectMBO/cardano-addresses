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
