# Contributing to cardano-addresses

If you find a bug or you'd like to propose a feature, please feel free to raise
an issue on our [issue tracker](https://github.com/IntersectMBO/cardano-addresses/issues).

Pull requests are welcome!

When creating a pull request, please make sure that your code adheres to our
[coding standards](https://github.com/input-output-hk/adrestia/blob/master/docs/code/Coding-Standards.md).

## Roles and Responsibilities

The `cardano-addresses` repository is co-maintained by @intersectmbo and @cardano-foundation.

The following people hold key responsibilities:

* @disassembler is responsible for releases
* @Crypto2099 is responsible for CI
* @paweljakubas is responsible for the Haskell components

Regular contributors for the Haskell components are

* @Anviking @paweljakubas @paolino

all of whom can merge PRs and be asked to review them.

In addition, the CODEOWNERS file identifies specific reviewers who are required for PRs that affect specific components.

## Versioning standard
**Semantic Versioning** is adopted in the repository. This means that if we have, for example, version *0.4.1*, then
public API exposed by `cardano-addresses` is ensured to stay the same until *0.5.0* version.
Hence, versions *0.4.2*, *0.4.3*,... *0.4.x* are patches
with regards to *0.4.1* and client code of `cardano-addresses` is expected to stay intact and functioning correctly.