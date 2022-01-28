{ pkgs, hls, ... }:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs; [ postgresql hls ghc cabal-install stack ];
    PGDATA = "./db";
    PGHOST = "localhost";
}
