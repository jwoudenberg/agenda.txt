{
  description = "agenda-txt";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        app = pkgs.haskellPackages.callCabal2nix "agenda-txt" ./. { };
      in {
        defaultPackage = pkgs.haskell.lib.justStaticExecutables app;
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ app ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.hpack
            pkgs.haskellPackages.shelltestrunner
            pkgs.ormolu
          ];
        };
      });
}
