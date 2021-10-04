{ mkShell, cardano-addresses-js, nodejs, nodePackages, ...}:
mkShell rec {
    name = "cardano-addresses-typescript-env-${version}";
    inherit (cardano-addresses-js) version;
    buildInputs = [ nodejs nodePackages.npm ];
    nobuildPhase = "touch $out";
    # The Typescript code running under nodejs will use this
    # environment variable to import jsapi code.
    CARDANO_ADDRESSES_JS = cardano-addresses-js;
  }
