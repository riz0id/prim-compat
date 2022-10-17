{ ghc ? "ghc924" }:

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
    llvm;
    
  haskell-language-server = pkgs."haskell-language-server-9.2.2";
}
