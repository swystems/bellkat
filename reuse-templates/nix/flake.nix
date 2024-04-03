{
  description = "Example for BellKAT";

  inputs.nixpkgs.url = "github:pschuprikov/nixpkgs/nixos-23.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.bellkat.url =
    "https://zenodo.org/records/10909730/files/bellkat-artifact.zip";

  outputs = { self, nixpkgs, bellkat, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = nixpkgs.lib;
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ bellkat.overlays.default ];
        };
      in {
        packages.default =
          pkgs.haskellPackages.callCabal2nix "my-example" ./. { };
        devShells.default = pkgs.haskellPackages.shellFor {
          buildInputs =
            [ pkgs.cabal-install pkgs.haskellPackages.haskell-language-server ];
          packages = ps: [ self.packages.${system}.default ];
        };
      });
}
