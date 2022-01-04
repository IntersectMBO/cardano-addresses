{
  description = "Cardano Addresses";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-2111";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, ... }: let
    inherit (nixpkgs) lib;
    supportedSystems = import ./nix/supported-systems.nix;
    defaultSystem = lib.head supportedSystems;

    overlays.haskell = final: prev: {
      cardanoAddressesHaskellProject = import ./nix/haskell.nix final.haskell-nix;
      inherit (final.cardanoAddressesHaskellProject.cardano-addresses-cli.components.exes) cardano-address;
      inherit (final.cardanoAddressesHaskellProject.projectCross.ghcjs.hsPkgs) cardano-addresses-jsapi;
    };
    overlays.js = final: prev: {
      cardano-addresses-js = final.callPackage ./nix/cardano-addresses-js.nix { };
      cardano-addresses-demo-js = final.callPackage ./nix/cardano-addresses-demo-js.nix { };
      cardano-addresses-js-shell = final.callPackage ./nix/cardano-addresses-js-shell.nix { };
    };
    overlays.deps = lib.composeManyExtensions [
      haskellNix.overlay
      iohkNix.overlays.utils
      iohkNix.overlays.crypto
      (final: prev: {
        cabal = final.haskell-nix.tool final.cardanoAddressesHaskellProject.pkg-set.config.compiler.nix-name "cabal" {
          version = "latest";
        };
      })
    ];
  in {
    inherit overlays;
    overlay = lib.composeExtensions overlays.haskell overlays.js;
  } // flake-utils.lib.eachSystem supportedSystems (system: let
    pkgs = import nixpkgs {
      inherit system;
      inherit (haskellNix) config;
      overlays = [
        overlays.deps
        overlays.haskell
        overlays.js
      ];
    };

    flake' = pkgs.cardanoAddressesHaskellProject.flake {
      crossPlatforms = p: with p; [ ghcjs ]
        ++ lib.optionals (system == "x86_64-linux") [
          mingwW64
          musl64
        ];
    };

    flake = lib.recursiveUpdate flake' {
      legacyPackages = pkgs;

      # Built by `nix build .`
      defaultPackage = flake.packages."cardano-addresses-cli:exe:cardano-address";

      # Run by `nix run .`
      defaultApp = flake.apps."cardano-addresses-cli:exe:cardano-address";

      packages = {
        inherit (pkgs)
          cardano-addresses-js
          cardano-addresses-demo-js
          cardano-addresses-js-shell;
      };

      devShells.js = pkgs.cardano-addresses-js-shell;

      apps = {
        repl = flake-utils.lib.mkApp {
          drv = pkgs.writeShellScriptBin "repl" ''
            confnix=$(mktemp)
            trap "rm -f $confnix" EXIT
            echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
            nix repl $confnix
          '';
        };
      };
    };
  in
    flake);
}
