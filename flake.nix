{
  description = "Cardano Addresses";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
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
    hackageGHCJS = {
      url = "github:input-output-hk/hackage-overlay-ghcjs?ref=repo";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, CHaP, hackageGHCJS, ... }:
    let
      inherit (nixpkgs) lib;
      inherit (flake-utils.lib) eachSystem mkApp;
      supportedSystems = import ./nix/supported-systems.nix;
      defaultSystem = lib.head supportedSystems;
      overlay = final: prev:
        {
          cardanoAddressesHaskellProject = self.legacyPackages.${final.system};
          inherit (final.cardanoAddressesHaskellProject.cardano-addresses-cli.components.exes) cardano-address;
          inherit (final.cardanoAddressesHaskellProject.projectVariants.ghc810.projectCross.ghcjs.hsPkgs) cardano-addresses-jsapi;
          inherit (self.packages.${final.system}) cardano-addresses-js cardano-addresses-demo-js;
        };
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
            inherit system CHaP hackageGHCJS;
            inherit (pkgs) haskell-nix;
          });

          cardano-addresses-js = pkgs.callPackage ./nix/cardano-addresses-js.nix { };
          cardano-addresses-demo-js = pkgs.callPackage ./nix/cardano-addresses-demo-js.nix { };
          cardano-addresses-js-shell = pkgs.callPackage ./nix/cardano-addresses-js-shell.nix { };

          flake = haskellProject.flake {};
        in
        lib.recursiveUpdate flake {
          hydraJobs.devShells = flake.devShells;

          legacyPackages = haskellProject;

          # Built by `nix build .`
          defaultPackage = flake.packages."cardano-addresses-cli:exe:cardano-address";

          # Run by `nix run .`
          defaultApp = flake.apps."cardano-addresses-cli:exe:cardano-address";

          packages = {
            inherit cardano-addresses-js cardano-addresses-demo-js cardano-addresses-js-shell;
          };

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
        }
      );
}
