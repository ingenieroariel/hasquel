{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  callHackageDirect = {pkg, ver, sha256}:
    let pkgver = "${pkg}-${ver}";
      in haskellPackages.callCabal2nix pkg (pkgs.fetchzip {
           url = "mirror://hackage/${pkgver}/${pkgver}.tar.gz";
           inherit sha256;
         });

  hmatrix = callHackageDirect {
      pkg = "hmatrix";
      ver = "0.20.0.0";
      sha256 = "1xzlp4ls0szw6nfzp4j2jzfllryzrkd0hsfr9q6sns2m78g609gl";
    } {};

  haskellDeps = ps: with ps; [
    base
    doctest
    matrix
    hmatrix
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;
in
pkgs.stdenv.mkDerivation rec {
  name = "hasquel";
  src = ./.;
  buildInputs = [ ghc ];
  buildPhase = ''
    ghc -o main hasquel.hs
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp main $out/bin/$name
  '';
  shellHooks = ''
    # This is needed for doctest to find the right ghc
    # FIXME: If we want this to stay, we would need to pin pkgs to a particular
    # commit in github.com/nixos/nixpkgs because the 8.6.4 number may change
    export GHC_PACKAGE_PATH=${ghc}/lib/ghc-8.6.4/package.conf.d/
  '';

}
