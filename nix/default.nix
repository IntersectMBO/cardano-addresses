{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
}:
let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  iohkNix = import sources.iohk-nix {};
  haskellNix = (import sources."haskell.nix" { inherit system sourcesOverride; }).nixpkgsArgs;
  # Use our own nixpkgs if it exists in sources.json.
  # Otherwise, take the pinned version from iohk-nix.
  nixpkgs = if (sources ? nixpkgs)
    then (builtins.trace "Not using IOHK default nixpkgs (use 'niv drop nixpkgs' to use default for better sharing)"
      sources.nixpkgs)
    else iohkNix.nixpkgs;

  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.overlays
    # iohkNix: nix utilities and niv:
    ++ iohkNix.overlays.iohkNix

    # Our haskell-nix-ified cabal project overlay:
    ++ [ (import ./pkgs.nix) ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.config // config;
  };

in pkgs
