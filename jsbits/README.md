# GHCJS Build of cardano-addresses

How to build `cardano-addresses` library, executables, and tests with
ghcjs.

## With Nix

Build and run CLI:

```terminal
$ nix-build release.nix -A ghcjs.haskellPackages.cardano-addresses-cli.components.exes.cardano-address
$ node ./result/bin/cardano-address.jsexe/all.js --help
$ node ./result/bin/cardano-address.jsexe/all.js recovery-phrase generate
```

Execute library unit tests:
```terminal
$ nix-build release.nix -A ghcjs.haskellPackages.cardano-addresses.checks.unit
```

## With Cabal in nix-shell

The `nix-shell` development environment provides
`js-unknown-ghcjs-cabal`, which is a cross-compiling Cabal for ghcjs.

Build and run CLI:

```terminal
$ nix-shell
[nix-shell:~/iohk/cardano-addresses]$ js-unknown-ghcjs-cabal --builddir=dist-ghcjs build all
...

[nix-shell:~/iohk/cardano-addresses]$ js-unknown-ghcjs-cabal --builddir=dist-ghcjs run cardano-addresses-cli:exe:cardano-address
...
/home/rodney/iohk/cardano-addresses/dist-ghcjs/build/wasm32-none/ghcjs-8.6.5/cardano-addresses-cli-3.3.0/x/cardano-address/build/cardano-address/cardano-address: createProcess: runInteractiveProcess: exec: does not exist (No such file or directory)
cardano-address: createProcess: runInteractiveProcess: exec: does not exist (No such file or directory)

[nix-shell:~/iohk/cardano-addresses]$ node dist-ghcjs/build/wasm32-none/ghcjs-8.6.5/cardano-addresses-cli-3.3.0/x/cardano-address/build/cardano-address/cardano-address.jsexe/all.js recovery-phrase generate
```

### Limitations

1. `js-unknown-ghcjs-cabal run` doesn't work ghcjs code needs to be
   interpreted with `nodejs`.

2. `js-unknown-ghcjs-cabal` is not yet able to automatically use the
   nix build of `jsbits/cardano-crypto.js`.

   Workaround is to find and copy it in manually:

   ```terminal
   $ cp --no-preserve=mode /nix/store/*cardano-addresses-jsbits jsbits/jsbits
   $ ln -s ../jsbits/jsbits core
   $ ln -s ../jsbits/jsbits command-line
   ```

## Without Nix

This is more difficult because you need to manually install correct
versions of build tools and dependencies.

Use the script `jsbits/emscripten/build.sh` to make
`cardano-crypto.js` and then install the `cardano-addresses-jsbits`
library with ghcjs Cabal.
