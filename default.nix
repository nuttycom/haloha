{ mkDerivation, stdenv, base, basement, bytestring, containers, cryptonite
, hedgehog, lens, memory, relude, text
, transformers, vector-sized, vector-space
}:
mkDerivation {
  pname = "haloha";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base basement bytestring containers cryptonite 
    hedgehog lens memory relude text 
    transformers vector-sized vector-space
  ];
  executableHaskellDepends = [
    base basement bytestring containers cryptonite 
    hedgehog lens memory relude text 
    transformers vector-sized vector-space
  ];
  testHaskellDepends = [
    base basement bytestring containers cryptonite 
    hedgehog lens memory relude text 
    transformers vector-sized vector-space
  ];
  homepage = "https://github.com/nuttycom/haloha#readme";
  license = stdenv.lib.licenses.bsd3;
}

