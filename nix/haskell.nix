############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ pkgs
, lib
, stdenv
, haskell-nix
, buildPackages
# GHC attribute name
, compiler
# Enable profiling
, profiling ? false
}:
let

  src = haskell-nix.haskellLib.cleanGit {
    name = "cardano-addresses-src";
    src = ../.;
  };

  isCrossBuild = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject ({ pkgs, ... }: {
    # FIXME: without this deprecated attribute, db-converter fails to compile directory with:
  } // {
    # Constraints not in `cabal.project.freeze for cross platform support
    cabalProjectLocal = ''
      packages:
        jsbits/cardano-addresses-jsbits.cabal
    '' + lib.optionalString stdenv.hostPlatform.isWindows ''
      constraints: Wind32 ==2.6.1.0, mintty ==0.1.2
    '';

    inherit src;
    compiler-nix-name = compiler;
    modules = [
      # Allow reinstallation of Win32
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
        nonReinstallablePkgs =
        [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
          "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
          # ghcjs custom packages
          "ghcjs-prim" "ghcjs-th"
          "ghc-boot"
          "ghc" "array" "binary" "bytestring" "containers"
          "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
          # "ghci" "haskeline"
          "hpc"
          "mtl" "parsec" "text" "transformers"
          "xhtml"
          # "stm" "terminfo"
        ];
      })
      ({ config, ... }: {
        packages.cardano-addresses.configureFlags = [ "--ghc-option=-Werror" ];
        packages.cardano-addresses-cli.configureFlags = [ "--ghc-option=-Werror" ];

        # This works around an issue with `cardano-addresses-cli.cabal`
        # Haskell.nix does not like `build-tool: cardano-address` as it looks in the
        # cardano-address package instead of the `cardano-addresses-cli`.
        # For some reason `cabal configure` fails if it is changed to:
        # `build-tool-depends: cardano-address-cli:cardano-address
        # Explicitly overriding the `build-tools` allows `build-tool: cardano-address`
        # for now.  A better fix would be to work out why cabal fails when
        # `build-tool-depends` is used.
        packages.cardano-addresses-cli.components.tests.unit.build-tools = pkgs.lib.mkForce [
          config.hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover
          config.hsPkgs.buildPackages.cardano-addresses-cli.components.exes.cardano-address
        ];
      })

      # Build fixes for Hackage dependencies.
      ({ pkgs, ...}: let
        pcre = pkgs.pcre.overrideAttrs (oldAttrs: {
          configureFlags = oldAttrs.configureFlags ++ [ "--enable-static" ];
        });
      in {
        # Needed for linking of the musl static build.
        packages.pcre-light.flags.use-pkg-config = true;
        packages.pcre-light.components.library.libs = [ pcre ];
        packages.cardano-addresses-cli.components.tests.unit.configureFlags =
          lib.mkIf stdenv.hostPlatform.isMusl
            [ "--ghc-option=-optl=-L${pcre}/lib" ];
      })

      (lib.mkIf isCrossBuild {
        # Remove hsc2hs build-tool dependencies (suitable version will
        # be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];

        # Make sure we use a buildPackages version of happy
        packages.pretty-show.components.library.build-tools = [
          buildPackages.haskell-nix.haskellPackages.happy
        ];

        # Disable cabal-doctest tests by turning off custom setups
        packages.comonad.package.buildType = lib.mkForce "Simple";
        packages.distributive.package.buildType = lib.mkForce "Simple";
        packages.lens.package.buildType = lib.mkForce "Simple";
        packages.nonempty-vector.package.buildType = lib.mkForce "Simple";
        packages.semigroupoids.package.buildType = lib.mkForce "Simple";
      })

      ({pkgs, config, ... }:
      let
        # Run the script to build the C sources from cryptonite and cardano-crypto
        # and place the result in jsbits/cardano-crypto.js
        jsbits = pkgs.runCommand "cardano-addresses-jsbits" {} ''
          script=$(mktemp -d)
          cp -r ${../jsbits/emscripten}/* $script
          ln -s ${pkgs.srcOnly {name = "cryptonite-src"; src = config.packages.cryptonite.src;}}/cbits $script/cryptonite
          ln -s ${pkgs.srcOnly {name = "cardano-crypto-src"; src = config.packages.cardano-crypto.src;}}/cbits $script/cardano-crypto
          patchShebangs $script/build.sh
          (cd $script && PATH=${
              # The extra buildPackages here is for closurecompiler.
              # Without it we get `unknown emulation for platform: js-unknown-ghcjs` errors.
              lib.makeBinPath (with pkgs.buildPackages.buildPackages;
                [emscripten closurecompiler coreutils])
            }:$PATH ./build.sh)
          mkdir -p $out
          cp $script/cardano-crypto.js $out
        '';
        addJsbits = ''
          mkdir -p jsbits
          cp ${jsbits}/* jsbits
        '';

      in lib.mkIf pkgs.stdenv.hostPlatform.isGhcjs {
        packages.digest.components.library.libs = lib.mkForce [ pkgs.buildPackages.zlib ];
        packages.cardano-addresses-cli.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];
        packages.cardano-addresses-jsbits.components.library.preConfigure = addJsbits;

        # Disable CLI running tests under ghcjs
        packages.cardano-addresses-cli.components.tests.unit.preCheck = ''
          echo "CLI tests disabled under ghcjs"
          exit 0
        '';
      })
    ];
  });
in
  pkgSet
