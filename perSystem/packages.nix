{
  perSystem = { hsPkgs, ... }:
    let
      ca = hsPkgs.cardano-addresses;
    in
    {
      packages.cardano-addresses = ca.components.library;
      packages.cardano-address = ca.components.exes.cardano-address;

      checks.unit = ca.components.tests.unit;
    };
}
