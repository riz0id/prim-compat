{ ghc }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    cabal-install
    haskell-language-server 
    prim-compat
    stylish-haskell;
    
  inherit (pkgs) 
    clang 
    llvm;
    
}
