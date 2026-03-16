{
  description = "Cardano Addresses";

  inputs = {
    nixpkgs.follows = "devx/nixpkgs";
    hostNixpkgs.follows = "nixpkgs";
    devx.url = "github:input-output-hk/devx";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, devx, hostNixpkgs, ... }:
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
            overlays = [ devx.overlays.default overlay ];
          };

          flake = devx.outputs.legacyPackages.${system}.flake;

          baseFlake = lib.recursiveUpdate flake {
            hydraJobs.devShells = flake.devShells;

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

          docker = if system == "x86_64-linux" then { } else { };
        in
        baseFlake // docker
      );
}
