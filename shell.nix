{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, containers, diagrams
      , diagrams-cairo, diagrams-contrib, diagrams-core, diagrams-lib
      , diagrams-solve, diagrams-svg, hxt, diagrams-graphviz
      , graphviz, stdenv, transformers, directory, text, fgl
      , linear, dynamic-plot
      }:
      mkDerivation {
        pname = "ling";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          array base containers diagrams diagrams-cairo diagrams-contrib
          diagrams-core diagrams-lib diagrams-solve diagrams-svg hxt
          diagrams-graphviz graphviz transformers directory text fgl
          linear dynamic-plot
        ];
        buildDepends = with pkgs; [
          cabal-install graphviz
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
