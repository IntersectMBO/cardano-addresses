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
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages cardanoAddressesHaskellPackages);

  self = {
    inherit haskellPackages;
    inherit (haskellPackages.cardano-addresses-cli.components.exes) cardano-address;

    # `tests` are the test suites which have been built.
    #tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    #benchmarks = collectComponents' "benchmarks" haskellPackages;

    #libs = collectComponents' "library" haskellPackages;

    exes = collectComponents' "exes" haskellPackages;

    #checks = recurseIntoAttrs {
    #  # `checks.tests` collect results of executing the tests:
    #  tests = collectChecks haskellPackages;
    #};
  };
in self
