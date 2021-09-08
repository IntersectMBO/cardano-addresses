{ pkgs ? import ./default.nix {}
, compiler ? "ghc8103"
}:

with pkgs;

mkShell rec {
  name = "cardano-addresses-env";
  meta.platforms = lib.platforms.unix;

  ghc = haskell.compiler.${compiler};

  tools = [
    ghc
    cabal-install
    stack
    nix
    pkgconfig
    bundler
  ] ++ lib.optional (!stdenv.isDarwin) git;

  libs = [
    bzip2
    libsodium-vrf
    lzma
    ncurses
    openssl
    pcre
    zlib
  ]
  ++ lib.optional (stdenv.hostPlatform.libc == "glibc") glibcLocales
  ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa CoreServices libcxx libiconv
  ]);

  libsodium-vrf = libsodium.overrideAttrs (oldAttrs: {
    name = "libsodium-1.0.18-vrf";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "libsodium";
      # branch tdammers/rebased-vrf
      rev = "66f017f16633f2060db25e17c170c2afa0f2a8a1";
      sha256 = "12g2wz3gyi69d87nipzqnq4xc6nky3xbmi2i2pb2hflddq8ck72f";
    };
    nativeBuildInputs = [ autoreconfHook ];
    configureFlags = "--enable-static";
  });

  buildInputs = tools ++ libs;

  phases = ["nobuildPhase"];
  nobuildPhase = "echo '${lib.concatStringsSep "\n" buildInputs}' > $out";
  preferLocalBuild = true;

  # Ensure that libz.so and other libraries are available to TH splices.
  LD_LIBRARY_PATH = lib.makeLibraryPath libs;

  # Force a UTF-8 locale because many Haskell programs and tests
  # assume this.
  LANG = "en_US.UTF-8";

  # Make the shell suitable for the stack nix integration
  # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
  GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  STACK_IN_NIX_SHELL = "true";
}
