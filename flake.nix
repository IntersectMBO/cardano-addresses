{
  description = "Cardano Addresses";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-2305";
    hostNixpkgs.follows = "nixpkgs";
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskellNix = {
      # GHC 8.10.7 cross compilation for windows is broken in newer versions of haskell.nix.
      # Unpin this once we no longer need GHC 8.10.7.
      url = "github:input-output-hk/haskell.nix/cb139fa956158397aa398186bb32dd26f7318784";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackageNix";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, CHaP, hostNixpkgs, ... }:
    let
      inherit (nixpkgs) lib;
      inherit (flake-utils.lib) eachSystem mkApp;
      supportedSystems = import ./nix/supported-systems.nix;
      defaultSystem = lib.head supportedSystems;
      overlay = final: prev: { };
    in
    {
      inherit overlay;
    } // eachSystem supportedSystems
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit (haskellNix) config;
            overlays = [
              haskellNix.overlay
              iohkNix.overlays.utils
              iohkNix.overlays.crypto
              (final: prev: {
                cabal = final.haskell-nix.tool haskellProject.pkg-set.config.compiler.nix-name "cabal" {
                  version = "latest";
                };
              })
              overlay
            ];
          };

          haskellProject = (import ./nix/haskell.nix {
            inherit system CHaP;
            inherit (pkgs) haskell-nix;
          });

          flake = haskellProject.flake {};

          baseFlake = lib.recursiveUpdate flake {
            hydraJobs.devShells = flake.devShells;

            legacyPackages = haskellProject;

            # Built by `nix build .`
            defaultPackage = flake.packages."cardano-addresses:exe:cardano-address";

            # Run by `nix run .`
            defaultApp = flake.apps."cardano-addresses:exe:cardano-address";

            apps = {
              repl = mkApp {
                drv = pkgs.writeShellScriptBin "repl" ''
                  confnix=$(mktemp)
                  echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
                  trap "rm $confnix" EXIT
                  nix repl $confnix
                '';
              };
            };
          };

          docker = if system == "x86_64-linux" then {
            packages.docker-image = pkgs.dockerTools.buildImage {
              name = "cardano-address";
              tag = haskellProject.version or "dev";
              config = { EntryPoint = [ "cardano-address" ]; };
              copyToRoot = pkgs.buildEnv {
                name = "image-root";
                paths = [ flake.packages."cardano-addresses:exe:cardano-address" pkgs.bash ];
              };
            };
          } else { packages = { }; };
        in
        baseFlake // docker
      )
    // {
      # Cross-compilation to Windows from Linux x86_64
      packages.x86_64-linux = let
        pkgs = import hostNixpkgs {
          system = "x86_64-linux";
          crossSystem = { config = "x86_64-w64-mingw32"; };
          overlays = [
            haskellNix.overlay
          ];
          config = {
            allowUnsupportedSystem = true;
          };
        };
        haskellProject = import ./nix/haskell.nix {
          system = "x86_64-linux";
          CHaP = CHaP;
          inherit (pkgs) haskell-nix;
        };
        crossFlake = haskellProject.flake { };
      in
        {
          default = crossFlake.packages."cardano-addresses:exe:cardano-address";
        };
    };
}
