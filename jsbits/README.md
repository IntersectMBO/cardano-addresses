# GHCJS Build of cardano-addresses

How to build `cardano-addresses` library, executables, and tests with
ghcjs.

## With Nix

Build and run CLI:

```terminal
$ nix build .#ghc810-javascript-unknown-ghcjs:cardano-addresses-cli:exe:cardano-address
$ ./result/bin/cardano-address --help
$ ./result/bin/cardano-address recovery-phrase generate
```

Execute library unit tests:
```terminal
$ nix build .#checks.x86_64-linux.ghc810-javascript-unknown-ghcjs:cardano-addresses:test:unit
$ cat result/test-stdout
```

## With Cabal in nix-shell

The `nix-shell` development environment provides
`js-unknown-ghcjs-cabal`, which is a cross-compiling Cabal for ghcjs.

Build and run CLI:

```terminal
$ nix develop
$ js-unknown-ghcjs-cabal --builddir=dist-ghcjs build all
...

$ js-unknown-ghcjs-cabal --builddir=dist-ghcjs run cardano-addresses-cli:exe:cardano-address
...
/home/rodney/iohk/cardano-addresses/dist-ghcjs/build/wasm32-none/ghcjs-8.6.5/cardano-addresses-cli-3.3.0/x/cardano-address/build/cardano-address/cardano-address: createProcess: runInteractiveProcess: exec: does not exist (No such file or directory)
cardano-address: createProcess: runInteractiveProcess: exec: does not exist (No such file or directory)

$ node dist-ghcjs/build/wasm32-none/ghcjs-8.6.5/cardano-addresses-cli-3.3.0/x/cardano-address/build/cardano-address/cardano-address.jsexe/all.js recovery-phrase generate
culture fringe exercise stumble gold current balance ....
```

### Limitations

1. `js-unknown-ghcjs-cabal run` doesn't work ghcjs code needs to be
   interpreted with `nodejs`.

2. We needed to add dummy calls to `Cardano.Address.Jsbits.addJsbitsDependency`
   to ensure that ghcjs linked in the emscripten-compiled crypto code.

## Without Nix

This is more difficult because you need to manually install correct
versions of build tools and dependencies.

Use the script `jsbits/emscripten/build.sh` to make
`cardano-crypto.js` and then install the `cardano-addresses-jsbits`
library with ghcjs Cabal.
