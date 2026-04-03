{
  description = "Cardano Addresses";

  inputs = {
    nixpkgs.follows = "devx/nixpkgs";
    devx.url = "github:input-output-hk/devx";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, devx, ... }:
    let
      inherit (nixpkgs) lib;
      inherit (flake-utils.lib) eachSystem mkApp;
      supportedSystems = import ./nix/supported-systems.nix;
    in
    {
      devShells = lib.genAttrs supportedSystems (system:
        devx.outputs.devShells.${system}
      );
    } // eachSystem supportedSystems
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          devShells.default = devx.outputs.devShells.${system}.ghc96-iog;

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
