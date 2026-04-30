{ inputs, ... }:

{
  perSystem = { pkgs, lib, ... }:
    let
      project = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }: {
        src = ./..;
        name = "cardano-addresses";
        compiler-nix-name = lib.mkDefault "ghc967";

        inputMap = {
          "https://chap.intersectmbo.org/" = inputs.CHaP;
        };

        modules = [{
          packages.basement.components.library.configureFlags = [
            "--hsc2hs-option=--cflag=-Wno-int-conversion"
          ];
        }];
      });
    in
    {
      _module.args.project = project;
      _module.args.hsPkgs = project.hsPkgs;
      _module.args.shellFor = args: project.shellFor args;
      legacyPackages.project = project;
    };
}
