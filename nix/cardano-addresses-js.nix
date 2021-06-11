{ lib, stdenv, nodejs, cardano-addresses-jsapi }:

stdenv.mkDerivation rec {
  name = "cardano-addresses-js-${version}";
  inherit (cardano-addresses-jsapi.identifier) version;
  src = lib.cleanSourceWith {
    name = "cardano-addresses-jsapi-glue";
    src = lib.sourceFilesBySuffices ../jsapi/glue [".js"];
  };
  jsexe = "${cardano-addresses-jsapi.components.exes.cardano-addresses-jsapi}/bin/cardano-addresses-jsapi.jsexe";
  buildPhase = ''
    print_files() {
      for js in "$@"; do
        name=$(basename $js)
        echo "// START $name"
        cat $js
        echo "// END $name"
        echo
      done
    }

    mkdir $out
    cd $out

    print_files $jsexe/rts.js $jsexe/lib.js $jsexe/out.js $src/runmain.js > cardano-addresses-jsapi.js

    for mod in cjs esm; do
      print_files $src/prelude.$mod.js cardano-addresses-jsapi.js $src/postlude.$mod.js > cardano-addresses-jsapi.$mod.js
    done
  '';
  doCheck = true;
  checkPhase = ''
    for js in $out/*.js; do
      if [[ ! $js =~ esm ]]; then
        ${nodejs}/bin/node --check $js
        echo "syntax check $js OK"
      fi
    done
  '';
  installPhase = "true";
  meta = {
    hydraPlatforms = [];
    platforms = lib.platforms.unix;
  };
  preferLocalBuild = true;
  allowSubstitutes = false;
}