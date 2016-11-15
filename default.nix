{ mkDerivation, base, constraints, stdenv }:
mkDerivation {
  pname = "constraint-categories";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base constraints ];
  description = "Synopsis";
  license = stdenv.lib.licenses.gpl3;
}
