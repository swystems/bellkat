{
  description = "Environment for playing with quantum";

  inputs.nixpkgs.url = "github:pschuprikov/nixpkgs/nixos-22.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.deploy-rs.url = "github:serokell/deploy-rs";

  outputs = { self, nixpkgs, flake-utils, deploy-rs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = nixpkgs.lib;
        ihaskell-diagrams-fix = self: super: {
          haskellPackages = super.haskellPackages.override {
            overrides = hself: hsuper: {
              ihaskell-diagrams = (super.haskell.lib.overrideSrc
                (super.haskell.lib.markUnbroken hsuper.ihaskell-diagrams) {
                  src = super.fetchFromGitHub {
                    owner = "IHaskell";
                    repo = "IHaskell";
                    rev = "c466b4fa2ad1d19db2733dfda83e4cf552c64f03";
                    sha256 = "K1uoUpzpOPN5WY/fNtlK2IOq4pW1jbAjosTob7fn9Og=";
                  };
                }).overrideAttrs (attrs:
                  let
                    script = ''
                      s/T.Encoding.decodeUtf8 imgData/T.unpack (&)/
                      /import qualified Data.Text/a \
                      import qualified Data.Text as T
                    '';
                  in {
                    patchPhase = ''
                      sed -i '${script}' IHaskell/Display/Diagrams.hs
                    '';
                    sourceRoot = "source/ihaskell-display/ihaskell-diagrams";
                  });
            };
          };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ihaskell-diagrams-fix ];
        };
        ihaskell = pkgs.ihaskell.override {
          packages = ps: [ ps.semirings ps.QuickCheck ps.ihaskell-diagrams ];
        };
      in {
        defaultPackage = pkgs.haskellPackages.callCabal2nix "qnkat-playground"
          (lib.sourceFilesBySuffices ./. [ ".hs" ".yaml" ]) { };
        devShell = pkgs.mkShell {
          buildInputs = [
            ihaskell
            deploy-rs.defaultPackage.${system}
            pkgs.ghcid
            pkgs.cabal-install
            pkgs.python3Packages.nbdime
            pkgs.haskellPackages.haskell-language-server
          ];
        };
      }) // (let system = "x86_64-linux";
      in {
        nixosConfigurations.quantum-server = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./configuration.nix
            (_: {
              nixpkgs.config.packageOverrides = pkgs: {
                haskellPackages = pkgs.haskellPackages.extend
                  (_: _: { qnkat-playground = self.defaultPackage.${system}; });
              };
            })
          ];
        };
        deploy = {
          sshUser = "pschuprikov";
          nodes.quantum-server = {
            hostname = "quantum.pschuprikov.me";
            profiles.system = {
              user = "root";
              path = deploy-rs.lib.${system}.activate.nixos
                self.nixosConfigurations.quantum-server;
            };
          };
        };
      });
}
