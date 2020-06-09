{ mkDerivation, base, directory, filepath, hpack, process, stdenv
}:
mkDerivation {
  pname = "zk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base directory filepath process ];
  testHaskellDepends = [ base ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/zk#readme";
  license = stdenv.lib.licenses.bsd3;
}
