{ lib, stdenv, nodejs, cardano-addresses-jsapi }:

stdenv.mkDerivation rec {
  name = "cardano-addresses-js-${version}";
  inherit (cardano-addresses-jsapi.identifier) version;
  srcs = [
    cardano-addresses-jsapi.components.exes.cardano-addresses-jsapi
    (lib.cleanSourceWith {
      name = "cardano-addresses-jsapi-glue";
      src = lib.sourceFilesBySuffices ../jsapi/glue [ ".js" ];
    })
  ];
  setSourceRoot = ''
    sourceRoot=$(echo */bin/cardano-addresses-jsapi.jsexe)
    glue=$(pwd)/$(echo *-glue)
    jsapi=$(pwd)/$sourceRoot
  '';
  sourceRoot = ".";
  postPatch = ''
    rm -f all.js

    sed_inplace() {
      local js="$1"
      shift
      cmd=(sed --in-place=.bak "$@" "$js")
      echo "''${cmd[@]}"
      "''${cmd[@]}"
      diff -Nur "$js".bak "$js" || true
    }

    sed_inplace out.js \
      -e '/function h\$integerzmwiredzminZCGHCziIntegerziTypezi\(Jn\|Jp\|S\)zh_con_e/,+3d'

    # Remove most of the double-return statements. Without this,
    # the sheer number of "unreachable code after return statement"
    # warnings slows down Firefox considerably!
    for js in rts.js out.js lib.js; do
      sed_inplace "$js" -e '/^  return.*;/!b;n;/^  return.*;/d'
    done
  '';
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

    print_files rts.js lib.js out.js $glue/runmain.js > $out/cardano-addresses-jsapi.js

    for mod in cjs esm; do
      print_files $glue/prelude.$mod.js $out/cardano-addresses-jsapi.js $glue/postlude.$mod.js > $out/cardano-addresses-jsapi.$mod.js
    done

    # nodejs needs the .mjs extension for ES modules
    cp --reflink=auto $out/cardano-addresses-jsapi.esm.js $out/cardano-addresses-jsapi.mjs
  '';
  doCheck = true;
  checkInputs = [ nodejs ];
  checkPhase = ''
    node --check $out/cardano-addresses-jsapi.js
    node --check $out/cardano-addresses-jsapi.cjs.js
    # node --experimental-modules --check $out/cardano-addresses-jsapi.mjs
    echo "syntax check OK"
  '';
  installPhase = "true";
  meta = {
    hydraPlatforms = [ ];
    platforms = lib.platforms.unix;
  };
  preferLocalBuild = true;
  allowSubstitutes = false;
}
