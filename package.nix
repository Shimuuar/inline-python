{ mkDerivation, base, bytestring, containers, exceptions, inline-c
, lib, primitive, process, python3-embed, quickcheck-instances, stm
, tasty, tasty-bench, tasty-hunit, tasty-quickcheck
, template-haskell, text, transformers, vector
}:
mkDerivation {
  pname = "inline-python";
  version = "0.1.1.1";
  sha256 = "169f3xa2v53h5pqkc6h34wswyl0y3311xq66jwshxh80ynp37nj3";
  libraryHaskellDepends = [
    base bytestring containers exceptions inline-c primitive process
    quickcheck-instances stm tasty tasty-hunit tasty-quickcheck
    template-haskell text transformers vector
  ];
  libraryPkgconfigDepends = [ python3-embed ];
  testHaskellDepends = [ base tasty ];
  benchmarkHaskellDepends = [ base tasty tasty-bench ];
  doHaddock = false;
  homepage = "https://github.com/Shimuuar/inline-python";
  description = "Python interpreter embedded into haskell";
  license = lib.licenses.bsd3;
}
