{ config, lib, withSystem, ... }: {
  flake.hydraJobs = lib.genAttrs config.systems (lib.flip withSystem (
    { config
    , pkgs
    , ...
    }:
    let
      isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
      required = lib.optionalAttrs (!isDarwin) { inherit (config) packages checks; };
      nonRequired = { inherit (config) devShells; }
        // lib.optionalAttrs isDarwin { inherit (config) packages checks; };
      jobs = required // nonRequired;
    in
    jobs
    // {
      required = pkgs.releaseTools.aggregate {
        name = "required";
        constituents = lib.collect lib.isDerivation required;
      };
      nonrequired = pkgs.releaseTools.aggregate {
        name = "nonrequired";
        constituents = lib.collect lib.isDerivation nonRequired;
      };
    }
  ));
}
