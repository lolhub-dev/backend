{ pkgs ? import <nixpkgs> {} }:
let
  sources = import ./nix/sources.nix;
  unstable = import sources.nixpkgs-unstable {};
in
pkgs.mkShell {
  buildInputs = with pkgs;[
    unstable.stack
    unstable.haskellPackages.haskell-language-server
    mongodb
  ];
  shellHook = ''
    mkdir -p mongodb/data/
    touch mongodb/log
  '';
}
