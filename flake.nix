{
  description = "Environment for playing with quantum";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.deploy-rs.url = "github:serokell/deploy-rs";

  outputs = { self, nixpkgs, flake-utils, deploy-rs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ihaskell = pkgs.ihaskell.override { packages = ps: [ ps.semirings ]; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = [
            ihaskell
            deploy-rs.defaultPackage.${system}
            pkgs.ghcid
            pkgs.cabal-install
          ];
        };
      }) // (let system = "x86_64-linux";
      in {
        nixosConfigurations.quantum-server = nixpkgs.lib.nixosSystem {
          inherit system;
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
