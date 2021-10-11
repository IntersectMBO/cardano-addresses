{ stdenv, cardano-addresses-js, buildPackages, lib, runtimeShell, python3, ... }:
stdenv.mkDerivation rec {
  name = "${pname}-${version}";
  pname = "cardano-addresses-demo-js";
  inherit (cardano-addresses-js) version;

  inherit (buildPackages) python3 runtimeShell;

  src = lib.cleanSourceWith {
    name = pname;
    src = lib.sourceFilesBySuffices ../jsapi [ ".js" ".map" ".html" ".in" ];
  };

  buildPhase = "true";
  installPhase = ''
    dest=$out/share/doc/${pname}
    mkdir -p $out/bin $dest

    if [ -d $src/dist ]; then
      install --mode=0644 -D --target-directory=$dest/dist \
          $src/dist/*.js $src/dist/*.map
    else
      echo "dist directory is missing - you need to 'npm run build' first"
      exit 1
    fi

    install --mode=0644 -D --target-directory=$dest \
      $src/demo/*.html $src/demo/*.js
    install --mode=0644 -D --target-directory=$dest/dist \
      ${cardano-addresses-js}/*

    cat > $out/bin/${pname} <<EOF
    #!${runtimeShell}
    cd ${placeholder "out"}
    exec ${python3}/bin/python -m http.server "$@"
    EOF
    chmod 755 "$out"/bin/*
  '';

  meta.platforms = lib.platforms.unix;
}
