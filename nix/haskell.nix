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

    compiler-nix-name = "ghc96";
    flake = {
      variants = {
        ghc810.compiler-nix-name = lib.mkForce "ghc810";
      };
      crossPlatforms = p: with p;
        lib.optional (system == "x86_64-linux") mingwW64 ++
        lib.optional (system == "x86_64-linux") musl64;
    };
    shell = {
      tools = lib.optionalAttrs (compareGhc "9.10" < 0) {
        haskell-language-server.src =
          if compareGhc "9" < 0
            then haskell-nix.sources."hls-2.2"
            else haskell-nix.sources."hls-2.8";
      };
      nativeBuildInputs = with pkgs.pkgsBuildBuild; [ nixWrapped cabalWrapped ];
      shellHook = ''
        export LANG=C.UTF-8
        export LC_ALL=C.UTF-8
      '';
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
        # This works around an issue with `cardano-addresses.cabal`
        # Haskell.nix does not like `build-tool: cardano-address` as it looks in the
        # cardano-address package instead of the `cardano-addresses`.
        # For some reason `cabal configure` fails if it is changed to:
        # `build-tool-depends: cardano-address:cardano-address
        # Explicitly overriding the `build-tools` allows `build-tool: cardano-address`
        # for now.  A better fix would be to work out why cabal fails when
        # `build-tool-depends` is used.
        packages.cardano-addresses.components.tests.unit.build-tools = pkgs.lib.mkForce [
          config.hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover
          config.hsPkgs.cardano-addresses.components.exes.cardano-address
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
          packages.cardano-addresses.components.tests.unit.configureFlags = [ "--ghc-option=-optl=-L${pcre}/lib" ];
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
    ];
  }
)
