with import <nixpkgs> {};
let
  packageOverrides = haskellPackages.override {
    overrides = self: super: {
      # old doctest
      pipes-group = haskell.lib.dontCheck super.pipes-group;
    };
  };
  sourceOverrides = packageOverrides.extend (haskell.lib.packageSourceOverrides {
    bimaps = ../Lib-bimaps;
    BiobaseENA = ../Lib-BiobaseENA;
    BiobaseTypes = ../Lib-BiobaseTypes;
    BiobaseXNA = ./.;
    DPutils = ../Lib-DPutils;
    ForestStructures = ../Lib-ForestStructures;
    PrimitiveArray = ../Lib-PrimitiveArray;
    SciBaseTypes = ../Lib-SciBaseTypes;
  });
in
sourceOverrides

