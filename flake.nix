{
  description = "The news-server flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
    easy-hls-nix.url = "github:jkachmar/easy-hls-nix";
  };
  outputs = { self, nixpkgs, flake-utils, easy-hls-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        hls = pkgs.callPackage easy-hls-nix { ghcVersions = [ "8.10.7" ]; };
      in {
        devShell = import ./shell.nix { inherit pkgs hls; };
      });
}
