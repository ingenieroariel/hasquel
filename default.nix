{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base
    split
    pipes
    doctest
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
