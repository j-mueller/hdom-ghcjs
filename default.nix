{ mkDerivation, base, containers, contravariant, ghcjs-base
, ghcjs-prim, hdom-api, lens, monad-supply, mtl, pipes, stdenv, stm
}:
mkDerivation {
  pname = "hdom-ghcjs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers contravariant ghcjs-base ghcjs-prim hdom-api lens
    monad-supply mtl pipes stm
  ];
  description = "vdom rendering in a web worker";
  license = stdenv.lib.licenses.bsd3;
}
