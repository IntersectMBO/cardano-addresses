{ inputs, ... }: {
  perSystem = { pkgs, system, lib, ... }:
    lib.optionalAttrs (system == "x86_64-linux") (
      let
        wasmProject = pkgs.haskell-nix.cabalProject' {
          src = ./..;
          cabalProjectFileName = "cabal-wasm.project";
          name = "cardano-addresses-wasm";
          compiler-nix-name = "ghc9123";

          inputMap = {
            "https://chap.intersectmbo.org/" = inputs.CHaP;
          };

          modules = [{
            packages.ram.components.library.configureFlags = [
              "--hsc2hs-option=--cflag=-D_WASI_EMULATED_MMAN"
              "--hsc2hs-option=--lflag=-lwasi-emulated-mman"
            ];
            packages.crypton.components.library.ghcOptions = [
              "-optc-D_WASI_EMULATED_MMAN"
              "-optc-DARGON2_NO_THREADS"
              "-optl-lwasi-emulated-mman"
            ];
          }];
        };
      in
      {
        packages.wasm =
          wasmProject.projectCross.wasi32.hsPkgs.cardano-addresses.components.exes.cardano-addresses-wasm;
      }
    );
}
