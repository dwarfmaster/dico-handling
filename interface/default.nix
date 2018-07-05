{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  packages = with pkgs; [ qt5.full cmake gnumake gcc pkgconfig boost sourceHighlight clang ];
in pkgs.stdenv.mkDerivation {
  name = "nodelearning";
  buildInputs =  packages;
}

