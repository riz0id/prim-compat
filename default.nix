{ ghc ? "ghc924" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    haskell-language-server 
    prim-compat;
    
  inherit (pkgs) 
    clang 
    llvm;
    
}
