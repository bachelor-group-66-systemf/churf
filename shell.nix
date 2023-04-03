let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/747927516efcb5e31ba03b7ff32f61f6d47e7d87.zip") { }; # pin the channel to ensure reproducibility!
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  withHoogle = true;
  modifier = drv:
  pkgs.haskell.lib.addBuildTools drv (
    (with pkgs; [ hlint 
                  haskell-language-server 
                  ghc 
                  jasmin 
                  llvmPackages_15.libllvm 
                  texlive.combined.scheme-full
                ])
    ++
    (with pkgs.haskellPackages; [ cabal-install
                                  stylish-haskell 
                                  BNFC 
                                  alex
                                  happy
                                ]));
}

