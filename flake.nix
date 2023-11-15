{
  description = "Environment for BellKAT";

  inputs.nixpkgs.url = "github:pschuprikov/nixpkgs/nixos-23.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.deploy-rs.url = "github:serokell/deploy-rs";

  outputs = { self, nixpkgs, flake-utils, deploy-rs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = nixpkgs.lib;
        pkgs = import nixpkgs {
          inherit system;
          overlays =
            [ self.overlays.ihaskell-diagrams-fix self.overlays.default ];
        };
        ihaskell-dev = pkgs.ihaskell.override {
          packages = _:
            self.packages.${system}.default.getCabalDeps.libraryHaskellDepends
            ++ self.packages.${system}.default.getCabalDeps.testHaskellDepends;
        };
        ihaskell = pkgs.ihaskell.override {
          packages = _: [ (pkgs.haskell.lib.dontCheck pkgs.haskellPackages.bellkat) ];
        };
      in {
        packages.default = pkgs.haskellPackages.bellkat;
        devShells.default = pkgs.mkShell {
          buildInputs = [
            ihaskell-dev
            deploy-rs.defaultPackage.${system}
            pkgs.ghcid
            pkgs.cabal-install
            pkgs.haskellPackages.haskell-language-server
          ];
        };

        devShells.bellkat = pkgs.mkShell {
          buildInputs = [ ihaskell ];
        };
      }) // (let system = "x86_64-linux";
      in {
        overlays = {
          default = final: prev: {
            haskellPackages = prev.haskellPackages.extend (hself: hsuper: {
              bellkat = prev.haskell.lib.overrideCabal (hself.callCabal2nix "bellkat"
                (prev.lib.sourceFilesBySuffices ./. [ ".hs" ".yaml" ])  {}) (attrs: { 
                  testFlags = ["--qc-max-size" "4" "--qc-max-success" "100"];
                });
            });
          };
          ihaskell-diagrams-fix = final: prev: {
            haskellPackages = prev.haskellPackages.override {
              overrides = hself: hsuper: {
                ihaskell-diagrams = (prev.haskell.lib.overrideSrc
                  (prev.haskell.lib.markUnbroken hsuper.ihaskell-diagrams) {
                    src = prev.fetchFromGitHub {
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
                      patchPhase =
                        "sed -i '${script}' IHaskell/Display/Diagrams.hs";
                      sourceRoot = "source/ihaskell-display/ihaskell-diagrams";
                    });
              };
            };
          };
        };

        nixosConfigurations.quantum-server = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { bellkat = self; };
          modules = [ ./configuration.nix ];
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
