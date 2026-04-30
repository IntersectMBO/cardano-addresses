{ inputs, ... }: {
  perSystem = { shellFor, pkgs, config, ... }: {
    devShells.default = shellFor {
      packages = p: [ p.cardano-addresses ];

      nativeBuildInputs = [
        pkgs.jq
        pkgs.gh
        config.treefmt.build.wrapper
      ];

      tools = {
        cabal = "latest";
        ghcid = "latest";
        haskell-language-server = {
          src = inputs.haskellNix.inputs."hls-2.10";
          configureArgs = "--disable-benchmarks --disable-tests";
        };
      };

      shellHook = ''
        export LANG="en_US.UTF-8"
      '';

      withHoogle = false;
    };
  };
}
