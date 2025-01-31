############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ cardano-addresses ? { outPath = ./.; rev = "abcdef"; }
  # The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
}:
let
  defaultNix = import cardano-addresses;
  linuxBuild = builtins.elem "x86_64-linux" supportedSystems;
  defaultSystem =
    if linuxBuild then "x86_64-linux"
    else builtins.head supportedSystems;
  inherit (defaultNix.legacyPackages.${builtins.currentSystem}) pkgs;
  inherit (pkgs) lib;
  inherit (defaultNix) packages checks devShell;
in
let

  collectJobs = a: lib.filterAttrs (s: _: builtins.elem s supportedSystems) a;

  # This file seems pointless, but it forces Hydra to re-evaluate
  # every commit. The side-effect of that is that Hydra reports build
  # status to GitHub for every commit, which we want, and it wouldn't
  # normally do.
  build-version = pkgs.writeText "revision.json" (builtins.toJSON
    { inherit (cardano-addresses) rev; });

  jobs = lib.genAttrs [ "packages" "checks" "devShell" ] (n: collectJobs defaultNix.${n})
    // {
    required = pkgs.releaseTools.aggregate {
      name = "github-required";
      meta.description = "All jobs required to pass CI";
      constituents = [ build-version ]
        ++ (
        let ps = jobs.packages.${defaultSystem}; in
        [
          ps."cardano-addresses:exe:cardano-address"
        ]
      )
        ++ (lib.optionals linuxBuild (
        let ps = jobs.packages."x86_64-linux"; in
        [
          ps."x86_64-w64-mingw32:cardano-addresses:exe:cardano-address"
          ps."x86_64-unknown-linux-musl:cardano-addresses:exe:cardano-address"
        ]
      ))
        ++ (lib.attrValues jobs.checks.${defaultSystem})
        ++ (lib.attrValues jobs.devShell);
    };
  };
in
jobs
