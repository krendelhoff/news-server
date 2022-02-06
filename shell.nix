{ pkgs, hls, ... }:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs.haskellPackages; [ pkgs.postgresql
                                                     pkgs.ghc
                                                     pkgs.cabal-install
                                                     pkgs.stack
                                                     hls
                                                   #  haskell-language-server
                                                     stylish-haskell
                                                     hoogle
                                                     nix-tree
                                                     apply-refact
                                                     hlint
                                                     graphmod
                                                     hasktags
                                                     hpack
                                                     implicit-hie
                                                   ];
    PGDATA = "./db";
    PGHOST = "localhost";
}
