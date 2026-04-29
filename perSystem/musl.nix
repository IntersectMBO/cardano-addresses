{ ... }: {
  perSystem = { project, system, lib, ... }:
    lib.optionalAttrs (system == "x86_64-linux") {
      packages.cardano-address-static =
        project.projectCross.musl64.hsPkgs.cardano-addresses.components.exes.cardano-address;
    };
}
