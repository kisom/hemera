{ mkDerivation, base, parsec, stdenv }:
mkDerivation {
  pname = "hemera";
  version = "0.1.0.0";
  src = /home/kyle/code/haskell/hemera;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base parsec ];
  description = "Hemera is a simple Scheme implemented in Haskell";
  license = stdenv.lib.licenses.mit;
}

