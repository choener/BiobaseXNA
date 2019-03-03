{ mkDerivation, aeson, attoparsec, base, bimaps, binary, BiobaseENA
, BiobaseTypes, bytes, bytestring, cereal, cereal-vector, cmdargs
, containers, csv, data-default, deepseq, file-embed
, ForestStructures, hashable, lens, mtl, primitive, PrimitiveArray
, QuickCheck, split, stdenv, tasty, tasty-quickcheck, tasty-th
, text, tuple, vector, vector-binary-instances, vector-th-unbox
}:
mkDerivation {
  pname = "BiobaseXNA";
  version = "0.11.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson attoparsec base bimaps binary BiobaseENA BiobaseTypes bytes
    bytestring cereal cereal-vector containers csv data-default deepseq
    file-embed ForestStructures hashable lens mtl primitive
    PrimitiveArray QuickCheck split text tuple vector
    vector-binary-instances vector-th-unbox
  ];
  executableHaskellDepends = [ base cmdargs ];
  testHaskellDepends = [
    base QuickCheck tasty tasty-quickcheck tasty-th vector
  ];
  homepage = "https://github.com/choener/BiobaseXNA";
  description = "Efficient RNA/DNA/Protein Primary/Secondary Structure";
  license = stdenv.lib.licenses.gpl3;
}
