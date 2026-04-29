{ ... }: {
  perSystem = { project, system, lib, ... }:
    lib.optionalAttrs (system == "x86_64-linux") (
      let
        # ghc967 makes ghc-iserv flaky for Windows TH cross-compilation.
        # Use ghc9122 which is known to work reliably (mirrors cardano-api's approach).
        windowsProject = project.appendModule { compiler-nix-name = lib.mkForce "ghc9122"; };
      in
      {
        packages.cardano-address-windows =
          windowsProject.projectCross.mingwW64.hsPkgs.cardano-addresses.components.exes.cardano-address;
      }
    );
}
