{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-address --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
}:
let
  inherit (pkgs.haskell-nix.haskellLib)
    selectProjectPackages
    collectComponents'
    collectChecks';

  haskellPackages = pkgs.lib.dontRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages pkgs.cardanoAddressesHaskellPackages);

  self = {
    inherit haskellPackages pkgs;
    inherit (haskellPackages.cardano-addresses-cli.components.exes) cardano-address;
    inherit (haskellPackages.cardano-addresses.components) library;

    # The built (but not executed) test suites.
    tests = collectComponents' "tests" haskellPackages;
    # The results of executing the tests.
    checks = collectChecks' haskellPackages;

    # Development environment with bonus ghcjs cross-compiler.
    shell = pkgs.cardanoAddressesHaskellPackages.shellFor {
      crossPlatforms = p: [ p.ghcjs ];
      tools.cabal = {};
      tools.hpack = {};
      buildInputs = [ pkgs.nodejs ];
      packages = ps: [ ps.cardano-addresses ps.cardano-addresses-cli ];
    };
  };
in self
