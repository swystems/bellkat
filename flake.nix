{
  description = "Environment for playing with quantum";

  inputs.nixpkgs.url = "github:pschuprikov/nixpkgs/nixos-22.11";
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
        ihaskell = pkgs.ihaskell.override {
          packages = _:
            self.defaultPackage.${system}.getCabalDeps.libraryHaskellDepends
            ++ self.defaultPackage.${system}.getCabalDeps.testHaskellDepends;
        };
      in {
        defaultPackage = pkgs.haskellPackages.qnkat-playground;
        devShell = pkgs.mkShell {
          buildInputs = [
            ihaskell
            deploy-rs.defaultPackage.${system}
            pkgs.ghcid
            pkgs.cabal-install
            pkgs.haskellPackages.haskell-language-server
          ];
        };
      }) // (let system = "x86_64-linux";
      in {
        overlays = {
          default = final: prev: {
            haskellPackages = prev.haskellPackages.extend (hself: hsuper: {
              qnkat-playground = prev.haskell.lib.overrideCabal (hself.callCabal2nix "qnkat-playground"
                (prev.lib.sourceFilesBySuffices ./. [ ".hs" ".yaml" ])  {}) (attrs: { 
                  testFlags = ["--skip" "parallel/should be commutative" "--qc-max-size" "4" "--qc-max-success" "10000"];
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
          specialArgs = { qnkat = self; };
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
