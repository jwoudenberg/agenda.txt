{
  description = "agenda-txt";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        agenda-txt =
          pkgs.haskellPackages.callCabal2nix "agenda-txt" ./agenda-txt { };
      in {
        packages.agenda-txt = pkgs.haskell.lib.justStaticExecutables agenda-txt;

        packages.ics-to-agenda-txt = pkgs.buildGoModule {
          pname = "ics-to-agenda-txt";
          version = "1.0.0";
          src = ./ics-to-agenda-txt;
          vendorHash = "sha256-whVHeCzFzadluptlXptVnCdWlZzSnYFG0sIuiJ1e8Sw=";
        };

        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ agenda-txt ];
          buildInputs = [
            # Haskell
            pkgs.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.hpack
            pkgs.haskellPackages.shelltestrunner
            pkgs.ormolu

            # Go
            pkgs.go
          ];
        };
      });
}
