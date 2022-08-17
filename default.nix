{ ghc ? "ghc922" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    fourmolu
    prim-compat;
    
  inherit (pkgs) 
    cabal-install 
    clang 
    haskell-language-server 
    llvm;
}