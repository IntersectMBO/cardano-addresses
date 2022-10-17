{
  description = "Cardano Addresses";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-2105";
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
    tullia = {
      url = "github:input-output-hk/tullia";
      # XXX uncomment once our version of nixpkgs has this fix:
      # https://github.com/NixOS/nixpkgs/commit/3fae68b30cfc27a7df5beaf9aaa7cb654edd8403
      # inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, tullia, ... }:
    let
      inherit (nixpkgs) lib;
      inherit (flake-utils.lib) eachSystem mkApp;
      supportedSystems = import ./nix/supported-systems.nix;
      defaultSystem = lib.head supportedSystems;
      overlay = final: prev:
        {
          cardanoAddressesHaskellProject = self.legacyPackages.${final.system};
          inherit (final.cardanoAddressesHaskellProject.cardano-addresses-cli.components.exes) cardano-address;
          inherit (final.cardanoAddressesHaskellProject.projectCross.ghcjs.hsPkgs) cardano-addresses-jsapi;
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

          haskellProject = (import ./nix/haskell.nix pkgs.haskell-nix);

          cardano-addresses-js = pkgs.callPackage ./nix/cardano-addresses-js.nix { };
          cardano-addresses-demo-js = pkgs.callPackage ./nix/cardano-addresses-demo-js.nix { };
          cardano-addresses-js-shell = pkgs.callPackage ./nix/cardano-addresses-js-shell.nix { };

          flake = haskellProject.flake {
            crossPlatforms = p: with p; [ ghcjs ]
            ++ (lib.optionals (system == "x86_64-linux") [
              mingwW64
              musl64
            ]);
          };

        in
        lib.recursiveUpdate flake {

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

          hydraJobs = import ./release.nix {
            cardano-addresses = self;
            supportedSystems = [ system ];
          };
        } //
          tullia.fromSimple system (import ./nix/tullia.nix)
      );

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
  };
}
