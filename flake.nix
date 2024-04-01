{
  description = "agenda-txt";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        agenda-txt = pkgs.haskellPackages.callCabal2nix "agenda-txt" ./agenda-txt { };
      in
      {
        defaultPackage = pkgs.haskell.lib.justStaticExecutables agenda-txt;
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ agenda-txt ];
          buildInputs = [
            # For Haskell
            pkgs.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.hpack
            pkgs.haskellPackages.shelltestrunner
            pkgs.ormolu
          ];
        };
      });
}
