{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, carray, deepseq, fft, gauge
      , JuicyPixels, massiv, split, stdenv, strict-concurrency, timeit
      , vector
      }:
      mkDerivation {
        pname = "AutoAstro";
        version = "0.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base carray deepseq fft JuicyPixels massiv split strict-concurrency
          timeit vector
        ];
        executableHaskellDepends = [
          base deepseq massiv strict-concurrency
        ];
        testHaskellDepends = [ base ];
        benchmarkHaskellDepends = [ base gauge ];
        description = "Automatic tools for improving astronomy images";
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
