let
  src = { outPath = ../.; };
  lock = builtins.fromJSON (builtins.readFile (src + "/flake.lock"));
  flake-compat = import (builtins.fetchTarball {
    url = "https://github.com/input-output-hk/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash;
  });
  pkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs-2311.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs-2311.locked.narHash;
    })
    { };
in
flake-compat {
  inherit src pkgs;
}
