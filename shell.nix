let
  pkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/8c619a1f3cedd16ea172146e30645e703d21bfc1.tar.gz) { }; # pin the channel to ensure reproducibility!
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  withHoogle = true;
  modifier = drv:
  pkgs.haskell.lib.addBuildTools drv (
    (with pkgs; [ hlint haskell-language-server ghc jasmin llvmPackages_15.libllvm])
    ++
    (with pkgs.haskellPackages; [ 
      cabal-install
      stylish-haskell 
      BNFC 
      alex
      happy
    ])
  );
}

