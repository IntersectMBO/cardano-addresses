############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ system, CHaP, haskell-nix }:
haskell-nix.cabalProject' (
  { pkgs
  , lib
  , config
  , ...
  }:
  let
    inherit (pkgs) stdenv;
    inherit (haskell-nix) haskellLib;
    # When adding a new Cabal package, or removing, update this attrset.
    # It's not automatically discovered from stack-pkgs yet.
    projectPackages = [
      "cardano-addresses"
      "cardano-addresses-cli"
      "cardano-addresses-jsapi"
      "cardano-addresses-jsbits"
    ];
    isCrossBuild = stdenv.hostPlatform != stdenv.buildPlatform;
    cabalProject = builtins.readFile ../cabal.project;
    compareGhc = builtins.compareVersions pkgs.buildPackages.haskell-nix.compiler.${config.compiler-nix-name}.version;
  in
  {
    src = haskell-nix.cleanSourceHaskell { name = "cardano-addresses-src"; src = ../.; };

    inputMap = {
      "https://chap.intersectmbo.org/" = CHaP;
    };

    # Setting this to builtins.currentSystem allows --impure to be used
    # to run evaluation on the current system.  For instance
    #   nix flake show --impure --allow-import-from-derivation
    # Falling back onto "x86_64-linux" should improve eval performance
    # on hydra.
    evalSystem = builtins.currentSystem or "x86_64-linux";

    # because src is filtered, (src + "./file") does not yet work with flake without https://github.com/NixOS/nix/pull/5163
    # So we avoid this idiom:
    inherit cabalProject;
    cabalProjectFreeze = null;
    cabalProjectLocal = ''
      -- Constraints not in `cabal.project.freeze for cross platform support
      packages:
        jsbits/cardano-addresses-jsbits.cabal
    '';

    compiler-nix-name = "ghc96";
    flake = {
      variants = {
        ghc810.compiler-nix-name = lib.mkForce "ghc810";
      };
      crossPlatforms = p: with p;
        lib.optional (system == "x86_64-linux" && builtins.elem config.compiler-nix-name ["ghc8107"]) ghcjs ++
        lib.optional (system == "x86_64-linux") musl64;
    };
    shell = {
      crossPlatforms = p: lib.optional (compareGhc "9.0" < 0) p.ghcjs;
      tools = lib.optionalAttrs (compareGhc "9.10" < 0) {
        haskell-language-server.src =
          if compareGhc "9" < 0
            then haskell-nix.sources."hls-2.2"
            else haskell-nix.sources."hls-2.8";
      };
      nativeBuildInputs = with pkgs.pkgsBuildBuild; [ nodejs nixWrapped cabalWrapped ];
      packages = ps:
        let
          projectPackages' = haskellLib.selectProjectPackages ps;
          currentProjectPackages = builtins.attrNames projectPackages';
        in
        if (currentProjectPackages != projectPackages)
        then
          throw "Project packages list has changed. Please update `projectPackages` in `nix/haskell.nix` to [\n \"${lib.concatStringsSep "\"\n \"" currentProjectPackages}\"\n]"
        else builtins.attrValues projectPackages';
    };

    modules = [
      ({ config, ... }: {
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
          config.hsPkgs.cardano-addresses-cli.components.exes.cardano-address
        ];
      })

      # Build fixes for Hackage dependencies.
      (lib.mkIf stdenv.hostPlatform.isMusl ({ pkgs, ... }:
        let
          pcre = pkgs.pcre.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ [ "--enable-static" ];
          }); in
        {
          # Needed for linking of the musl static build.
          packages.pcre-light.flags.use-pkg-config = true;
          packages.pcre-light.components.library.libs = [ pcre ];
          packages.cardano-addresses-cli.components.tests.unit.configureFlags = [ "--ghc-option=-optl=-L${pcre}/lib" ];
        }))

      (lib.mkIf isCrossBuild ({ pkgs, ... }: {
        # Remove hsc2hs build-tool dependencies (suitable version will
        # be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [ ];
        packages.terminal-size.components.library.build-tools = lib.mkForce [ ];
        packages.network.components.library.build-tools = lib.mkForce [ ];

        # Make sure we use a buildPackages version of happy
        packages.pretty-show.components.library.build-tools = [
          pkgs.buildPackages.haskell-nix.haskellPackages.happy
        ];

        # Disable cabal-doctest tests by turning off custom setups
        packages.pretty-simple.package.buildType = lib.mkForce "Simple";
        packages.comonad.package.buildType = lib.mkForce "Simple";
        packages.distributive.package.buildType = lib.mkForce "Simple";
        packages.lens.package.buildType = lib.mkForce "Simple";
        packages.nonempty-vector.package.buildType = lib.mkForce "Simple";
        packages.semigroupoids.package.buildType = lib.mkForce "Simple";
      }))

      # GHCJS build configuration
      ({ config, pkgs, ... }:
        let
          # Run the script to build the C sources from cryptonite and cardano-crypto
          # and place the result in jsbits/cardano-crypto.js
          jsbits = pkgs.runCommand "cardano-addresses-jsbits" { } ''
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
        in
        lib.mkIf pkgs.stdenv.hostPlatform.isGhcjs {
          reinstallableLibGhc = false;

          # TODO replace this with `zlib` build with `emcc` if possible.
          # Replace zlib with a derivation including just the header files
          packages.digest.components.library.libs = lib.mkForce [(
            pkgs.pkgsBuildBuild.runCommand "zlib" { nativeBuildInputs = [ pkgs.pkgsBuildBuild.xorg.lndir ]; } ''
              mkdir -p $out/include
              lndir ${pkgs.pkgsBuildBuild.lib.getDev pkgs.pkgsBuildBuild.zlib}/include $out/include
          '')];
          # Prevent downstream packages from looking for zlib
          packages.digest.components.library.postInstall = ''
            sed -i 's/^extra-libraries: *z//g' $out/package.conf.d/digest-*.conf
          '';
          # Prevent errors from missing zlib function _adler32
          packages.cardano-addresses.configureFlags = [ "--gcc-options=-Wno-undefined" ];
          packages.cardano-addresses-cli.configureFlags = [ "--gcc-options=-Wno-undefined" ];
          packages.cardano-addresses-jsapi.configureFlags = [ "--gcc-options=-Wno-undefined" ];

          packages.cardano-addresses-cli.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];
          packages.cardano-addresses-jsapi.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];
          packages.cardano-addresses-jsbits.components.library.preConfigure = addJsbits;
          packages.cardano-addresses-cli.components.tests.unit.preCheck = ''
            export CARDANO_ADDRESSES_CLI="${config.hsPkgs.cardano-addresses-cli.components.exes.cardano-address}/bin"
          '';
          packages.cardano-addresses-cli.components.tests.unit.build-tools = pkgs.lib.mkForce [
            config.hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover
            pkgs.buildPackages.nodejs
          ];
        })
      ({ pkgs, ... }: lib.mkIf (!pkgs.stdenv.hostPlatform.isGhcjs) {
          # Disable jsapi-test on jsaddle/native. It's not working yet.
          packages.cardano-addresses-jsapi.components.tests.jsapi-test.preCheck = ''
            echo "Tests disabled on non-ghcjs"
            exit 0
          '';
        })
    ];
  }
)
