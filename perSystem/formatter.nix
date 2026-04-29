{
  perSystem =
    { config
    , pkgs
    , inputs'
    , ...
    }: {
      # Treefmt-nix configuration
      treefmt.config = {
        projectRootFile = "flake.nix";
        # commented out because changes every file in repo
        #programs.stylish-haskell.enable = true;
        programs.hlint.enable = true;
        programs.nixpkgs-fmt.enable = true;
      };
    };
}
