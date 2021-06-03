{ pkgs ? import ../nix/default.nix {} }:

rec {
  cardano-addresses-js = pkgs.callPackage ../nix/cardano-addresses-js.nix {
    inherit (pkgs.cardanoAddressesHaskellPackages.projectCross.ghcjs.hsPkgs) cardano-addresses-jsapi;
  };

  shell = pkgs.mkShell rec {
    name = "cardano-addresses-typescript-env-${version}";
    inherit (cardano-addresses-js) version;
    buildInputs = with pkgs; [ nodejs nodePackages.npm ];
    nobuildPhase = "touch $out";
    # The Typescript code running under nodejs will use this
    # environment variable to import jsapi code.
    CARDANO_ADDRESSES_JS = cardano-addresses-js;
  };

  demo = pkgs.stdenv.mkDerivation rec {
    name = "${pname}-${version}";
    pname = "cardano-addresses-demo-js";
    inherit (cardano-addresses-js) version;

    inherit (pkgs.buildPackages) python3 runtimeShell;

    src = pkgs.lib.cleanSourceWith {
      name = pname;
      src = pkgs.lib.sourceFilesBySuffices ./. [".js" ".map" ".html" ".in"];
    };

    buildPhase = "true";
    installPhase = ''
      dest=$out/share/doc/${pname}
      mkdir -p $out/bin $dest
      install --mode=0644 -D --target-directory=$dest \
        $src/demo/*.html $src/demo/*.js
      if [ -d $src/dist ]; then
        install --mode=0644 -D --target-directory=$dest/dist \
          $src/dist/*.js $src/dist/*.map
      fi
      install --mode=0644 -D --target-directory=$dest/dist ${cardano-addresses-js}/*
      export local_build=${toString ./.}
      substituteAll "$src/demo/run.sh.in" "$out/bin/${pname}"
      chmod 755 "$out"/bin/*
    '';

    meta.platforms = pkgs.lib.platforms.unix;
  };
}
