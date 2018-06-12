with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsPkgs = haskellPackages.extend (packageSourceOverrides {
    bimaps = ../Lib-bimaps;
    BiobaseENA = ../Lib-BiobaseENA;
    BiobaseTypes = ../Lib-BiobaseTypes;
    BiobaseXNA = ./.;
    DPutils = ../Lib-DPutils;
    ForestStructures = ../Lib-ForestStructures;
    OrderedBits = ../Lib-OrderedBits;
    PrimitiveArray = ../Lib-PrimitiveArray;
    SciBaseTypes = ../Lib-SciBaseTypes;
  });
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.BiobaseXNA ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      BiobaseENA BiobaseTypes
      DPutils
      ForestStructures
      OrderedBits
      PrimitiveArray
      SciBaseTypes
    ];
  };
}
