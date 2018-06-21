with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = (lib.foldl' (s: p: s // (import p).hsSrcSet) {} [
    ../Lib-bimaps
    ../Lib-BiobaseENA
    ../Lib-BiobaseTypes
    ../Lib-ForestStructures
    ../Lib-PrimitiveArray
  ]) // {BiobaseXNA = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.BiobaseXNA ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      bimaps
      BiobaseENA
      BiobaseTypes
      ForestStructures
      PrimitiveArray
    ];
  };
}
