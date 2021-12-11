{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    haskellPackages.ghc 
    haskellPackages.cabal-install
    haskellPackages.pointfree
    haskellPackages.haskell-language-server
    ormolu
    hlint
    readline
  ];
}

