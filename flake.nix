{
  description = "Environment for BellKAT";

  inputs.nixpkgs.url = "github:pschuprikov/nixpkgs/nixos-23.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = nixpkgs.lib;
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
      in {
        packages.default = pkgs.haskellPackages.bellkat;
        packages.bellkatGHC = pkgs.haskellPackages.ghcWithPackages (ps : [ ps.bellkat ]);
        packages.bellkatGHCWithFC = pkgs.runCommand "bellkatghc-with-fc" {
          buildInputs = [ pkgs.makeWrapper ];
        } ''
          mkdir -p $out/bin
          makeWrapper ${self.packages.${system}.bellkatGHC}/bin/runhaskell $out/bin/runhaskell \
            --set FONTCONFIG_FILE "${pkgs.fontconfig.out}/etc/fonts/fonts.conf" \
            --set FONTCONFIG_PATH "${pkgs.fontconfig.out}/etc/fonts/"
        '';
        devShells.default = pkgs.haskellPackages.shellFor {
          FONTCONFIG_FILE = "${pkgs.fontconfig.out}/etc/fonts/fonts.conf";
          FONTCONFIG_PATH = "${pkgs.fontconfig.out}/etc/fonts/";
          buildInputs = [
            pkgs.ghcid
            pkgs.cabal-install
            pkgs.haskellPackages.haskell-language-server
          ];
          packages = ps: [ ps.bellkat ];
        };
      }) // {
        overlays = {
          default = final: prev: {
            haskellPackages = prev.haskellPackages.extend (hself: hsuper: {
              bellkat = prev.haskell.lib.overrideCabal
                (hself.callCabal2nix "bellkat"
                  (prev.lib.sourceFilesBySuffices ./. [ ".hs" ".yaml" ]) { })
                (attrs: {
                  testFlags = [ "--qc-max-size" "4" "--qc-max-success" "100" ];
                });
            });
          };
        };
      };
}
