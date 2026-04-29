{ ... }:
{
  perSystem = { hsPkgs, pkgs, ... }:
    let
      localPkgs = pkgs.haskell-nix.haskellLib.selectLocalPackages hsPkgs;
      haddockDocs = pkgs.lib.attrValues (
        pkgs.lib.mapAttrs (_: pkg: pkg.components.library.haddock.doc)
          (pkgs.lib.filterAttrs (_: pkg: pkg.components ? library) localPkgs)
      );
    in
    {
      packages.haddocks = pkgs.runCommand "haddocks"
        { buildInputs = [ pkgs.haskell-nix.compiler.ghc967 ]; }
        ''
          mkdir -p $out

          interfaceArgs=""
          for doc in ${toString haddockDocs}; do
            htmlDir=$(find "$doc/share/doc" -name "html" -type d | head -1)
            pkgId=$(basename "$(dirname "$htmlDir")")
            ifaceFile=$(find "$htmlDir" -maxdepth 1 -name "*.haddock" | head -1)

            mkdir -p "$out/$pkgId"
            cp -r "$htmlDir/." "$out/$pkgId/"

            interfaceArgs="$interfaceArgs --read-interface=$pkgId,$ifaceFile"
          done

          haddock --gen-index --gen-contents $interfaceArgs -o $out
        '';
    };
}
