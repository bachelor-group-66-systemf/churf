let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  withHoogle = true;
  modifier = drv:
  pkgs.haskell.lib.addBuildTools drv (
    (with pkgs; [ hlint haskell-language-server ghc jasmin ])
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

