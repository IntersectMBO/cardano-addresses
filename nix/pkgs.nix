# our packages overlay
pkgs: _: with pkgs; {
  cardanoAddressesHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
