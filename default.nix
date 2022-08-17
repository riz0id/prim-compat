{ ghc ? "ghc922" }:

let
  # nixpkgs release 22.05 pinned on August 17th, 2022.
  # url: <https://github.com/NixOS/nixpkgs/releases/tag/22.05>
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.05.tar.gz";
    sha256 = "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik";
  }; 

  pkgs = import nixpkgs {
    config.packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${ghc}" = pkgs.haskell.packages."${ghc}".extend (self: _: {
            prim-compat = self.callCabal2nix "prim-compat" ./. { };
          });
        };
      };
    }; 
  };

in {
  inherit (pkgs.haskell.packages."${ghc}") prim-compat;
  inherit (pkgs) cabal-install clang haskell-language-server llvm;
}
