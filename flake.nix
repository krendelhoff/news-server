{
  description = "The best flake";

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

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = throw "put your package name here!";
        hls = pkgs.callPackage easy-hls-nix { ghcVersions = [ "8.10.7" ]; };
      in {
#        packages.${packageName} = # (ref:haskell-package-def)
#          haskellPackages.callCabal2nix packageName self rec {
#            # Dependency overrides go here
#          };
#
#        defaultPackage = self.packages.${system}.${packageName};

        devShell = import ./shell.nix { inherit pkgs hls; }; # вот хули здесь нет атрибута packages? надо разбираться
                                                             # хули разбираться - посмотри исходники
                                                             # или поищи как пользуются другие бройлер
      });
}
