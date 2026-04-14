# Build cardano-addresses to WASM using GHC's WASM backend.
#
# Uses haskell.nix's index pinning for deterministic Hackage/CHaP access:
#   1. Fetch + truncate indices at pinned index-state (deterministic, from hackage.nix)
#   2. Bootstrap cabal package cache via mkLocalHackageRepo + cabal v2-update
#   3. Download package tarballs via wasm32-wasi-cabal --only-download (FOD)
#   4. Build WASM offline from cached deps (regular derivation)
{ pkgs
, ghcWasmToolchain
, src
, dependenciesHash
}:

let
  haskell-nix = pkgs.haskell-nix;
  projectFile = "cabal-wasm.project";

  # Index-state pins (must match cabal-wasm.project)
  hackageIndexState = "2026-03-30T00:00:00Z";
  # Note: the project's index-state must be 1 day BEFORE this
  # because truncate-index cuts at the boundary, and the last entry
  # before midnight may be hours earlier.
  chapIndexState = "2025-01-23T00:00:00Z";
  chapIndexHash = "e091628ae447eb3f40c27fef701b735f5eb9921fd02918a77a0557fec291f01f";

  # --- Step 1: Deterministic truncated indices ---

  truncatedHackageIndex = pkgs.fetchurl {
    name = "01-index.tar.gz-at-${hackageIndexState}";
    url = "https://hackage.haskell.org/01-index.tar.gz";
    downloadToTemp = true;
    postFetch = ''
      ${haskell-nix.nix-tools}/bin/truncate-index \
        -o $out -i $downloadedFile -s '${hackageIndexState}'
    '';
    outputHashAlgo = "sha256";
    outputHash = (import haskell-nix.indexStateHashesPath).${hackageIndexState};
  };

  truncatedChapIndex = pkgs.fetchurl {
    name = "chap-01-index.tar.gz-at-${chapIndexState}";
    url = "https://chap.intersectmbo.org/01-index.tar.gz";
    downloadToTemp = true;
    postFetch = ''
      ${haskell-nix.nix-tools}/bin/truncate-index \
        -o $out -i $downloadedFile -s '${chapIndexState}'
    '';
    outputHashAlgo = "sha256";
    outputHash = chapIndexHash;
  };

  mkLocalHackageRepo = haskell-nix.mkLocalHackageRepo;

  # --- Step 2: Bootstrap cabal package cache ---
  # (copied from haskell.nix bootstrapIndexTarball)

  bootstrapIndexTarball = name: index: pkgs.runCommand "cabal-bootstrap-${name}" {
    nativeBuildInputs = [ haskell-nix.nix-tools.exes.cabal ]
      ++ haskell-nix.cabal-issue-8352-workaround;
  } ''
    HOME=$(mktemp -d)
    mkdir -p $HOME/.cabal/packages/${name}
    cat <<EOF > $HOME/.cabal/config
    repository ${name}
      url: file:${mkLocalHackageRepo { inherit name index; }}
      secure: True
      root-keys: aaa
      key-threshold: 0
    EOF
    cabal v2-update ${name}
    cp -r $HOME/.cabal/packages/${name} $out
  '';

  bootstrappedHackage = bootstrapIndexTarball "hackage.haskell.org" truncatedHackageIndex;
  bootstrappedChap = bootstrapIndexTarball "cardano-haskell-packages" truncatedChapIndex;

  # --- Step 3: Assemble dotCabal ---

  dotCabal = pkgs.runCommand "dot-cabal-wasm" {
    nativeBuildInputs = [ pkgs.xorg.lndir ];
  } ''
    mkdir -p $out/packages/hackage.haskell.org
    lndir ${bootstrappedHackage} $out/packages/hackage.haskell.org

    mkdir -p $out/packages/cardano-haskell-packages
    lndir ${bootstrappedChap} $out/packages/cardano-haskell-packages

    cat > $out/config <<EOF
    repository hackage.haskell.org
      url: http://hackage.haskell.org/
      secure: True

    repository cardano-haskell-packages
      url: http://cardano-haskell-packages/
      secure: True

    executable-stripping: False
    shared: True
    EOF
  '';

  # --- Git source-repository-package deps (deterministic via fetchgit) ---

  cborg-src = pkgs.fetchgit {
    url = "https://github.com/well-typed/cborg.git";
    rev = "72a0e736e24c864b5a9b95d90adb37a9e8e6d761";
    hash = "sha256-SDzMk6gWXelE3OH6gCC6XSn+h5VbrKpaisyza9bCtVM=";
  };

  ram-src = pkgs.fetchgit {
    url = "https://github.com/paolino/ram.git";
    rev = "e6d863d240246e0a1af3dd12cff7047f696f81ea";
    hash = "sha256-hqBp5+Ti3bzGSR+JKRl7u7fbXb11L/kw2k0Pguq0xIM=";
  };

  srcMetadata = pkgs.lib.cleanSourceWith {
    inherit src;
    filter = name: type:
      let baseName = baseNameOf (toString name); in
      type == "directory" ||
      pkgs.lib.hasSuffix ".cabal" baseName ||
      baseName == projectFile;
  };

  # --- Step 4: Download tarballs (FOD) ---

  deps = pkgs.stdenv.mkDerivation {
    pname = "cardano-addresses-wasm-deps";
    version = "0.1.0";
    src = srcMetadata;

    nativeBuildInputs = [
      ghcWasmToolchain
      pkgs.cacert
      pkgs.git
      pkgs.curl
    ];

    buildPhase = ''
      export HOME=$NIX_BUILD_TOP/home
      mkdir -p $HOME
      export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
      export CURL_CA_BUNDLE=$SSL_CERT_FILE

      # Point wasm32-wasi-cabal at the bootstrapped dotCabal
      export CABAL_DIR=$NIX_BUILD_TOP/cabal
      mkdir -p $CABAL_DIR
      cp -rL ${dotCabal}/* $CABAL_DIR/
      chmod -R u+w $CABAL_DIR

      # Download tarballs
      wasm32-wasi-cabal --project-file=${projectFile} build \
        --only-download cardano-addresses-wasm
    '';

    installPhase = ''
      mkdir -p $out
      cp -r $CABAL_DIR/* $out/

      # Strip non-deterministic files
      find $out -name 'hackage-security-lock' -delete
      find $out -name '01-index.timestamp' -delete
    '';

    outputHashMode = "recursive";
    outputHash = dependenciesHash;
  };

  # --- Step 5: Build WASM offline ---

  wasm = pkgs.stdenv.mkDerivation {
    pname = "cardano-addresses-wasm";
    version = "0.1.0";
    inherit src;

    nativeBuildInputs = [ ghcWasmToolchain pkgs.git ];

    configurePhase = ''
      export HOME=$NIX_BUILD_TOP/home
      mkdir -p $HOME

      export CABAL_DIR=$NIX_BUILD_TOP/cabal
      mkdir -p $CABAL_DIR
      cp -rL ${deps}/* $CABAL_DIR/
      chmod -R u+w $CABAL_DIR

      # Replace source-repository-package entries with nix fetchgit paths
      cp ${projectFile} ${projectFile}.orig
      sed -i '/^source-repository-package/,/^$/d' ${projectFile}
      cat >> ${projectFile} <<EOF

      packages:
        cardano-addresses.cabal
        ${cborg-src}/cborg/cborg.cabal
        ${ram-src}/ram.cabal
      EOF
    '';

    buildPhase = ''
      export CABAL_DIR=$NIX_BUILD_TOP/cabal
      wasm32-wasi-cabal --project-file=${projectFile} build cardano-addresses-wasm
    '';

    installPhase = ''
      mkdir -p $out
      find dist-newstyle -name "cardano-addresses-wasm.wasm" -type f \
        -exec cp {} $out/cardano-addresses.wasm \;
    '';
  };

in {
  inherit deps wasm;
}
