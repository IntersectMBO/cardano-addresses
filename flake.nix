{
  description = "Cardano Addresses";

  inputs = {
    nixpkgs.follows = "devx/nixpkgs";
    devx.url = "github:input-output-hk/devx";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    ghc-wasm-meta = {
      url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
      flake = true;
    };
  };

  outputs = { self, nixpkgs, flake-utils, devx, haskellNix, ghc-wasm-meta, ... }:
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
          pkgs = import nixpkgs {
            inherit system;
          };
        in
        {
          devShells.default = devx.outputs.devShells.${system}.ghc96-iog;

          packages =
            let
              # Limit haskell.nix to the WASM packaging path only.
              pkgsWasm = import nixpkgs {
                inherit system;
                overlays = [ haskellNix.overlay ];
              };
              wasmBuild = import ./nix/wasm.nix {
                pkgs = pkgsWasm;
                ghcWasmToolchain = ghc-wasm-meta.packages.${system}.all_9_12;
                src = ./.;
                dependenciesHash = "sha256-tn90dlUhluIKWvHcxm8S8nBp06ysqHY5cUgKiLjmwJc=";
              };
            in
            {
              wasm = wasmBuild.wasm;
              wasm-deps = wasmBuild.deps;
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
