{ ghc ? "ghc924" }:

let 
  pkgs = import ./default.nix {
    inherit ghc;
  };
in pkgs.prim-compat.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [
    pkgs.clang
    pkgs.llvm
  ];
})
