{ ghc ? "ghc922" }:

let 
  nixpkgs = import ./nixpkgs.nix { };
in import nixpkgs {
  config.packageOverrides = pkgs: {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        "${ghc}" = pkgs.haskell.packages."${ghc}".extend (self: _: {
          prim-compat = self.callCabal2nix "prim-compat" ../. { };
        });
      };
    };
  }; 
}