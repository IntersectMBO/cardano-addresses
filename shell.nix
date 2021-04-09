with (import ./. {});
cardanoAddressesHaskellPackages.shellFor {
  crossPlatforms = p: [ p.ghcjs ];
  tools.cabal = {};
  tools.hpack = {};
  buildInputs = [ pkgs.nodejs ];
  packages = ps: [ ps.cardano-addresses ps.cardano-addresses-cli ];
}
