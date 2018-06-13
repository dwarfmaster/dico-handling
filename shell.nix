{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, ast-monad, ast-monad-json, base, containers
      , directory, fgl, graphviz, hxt, linear, mtl, stdenv, text
      , transformers
      }:
      mkDerivation {
        pname = "ling";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          array ast-monad-json base containers directory fgl graphviz hxt
          linear mtl text transformers ast-monad
        ];
        buildDepends = with pkgs; [
          graphviz cabal-install
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
