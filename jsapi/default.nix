let
  defaultNix = import ../default.nix;
  inherit (defaultNix.packages.${builtins.currentSystem}) cardano-addresses-js cardano-addresses-demo-js cardano-addresses-js-shell;
in
{
  inherit cardano-addresses-js;

  shell = cardano-addresses-js-shell;

  demo = cardano-addresses-demo-js;
}
