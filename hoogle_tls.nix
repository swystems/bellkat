{ lib, config, pkgs, ... }: 
with lib;
let 
  cfg = config.services.hoogle;
  hoogleEnv = pkgs.buildEnv {
    name = "hoogle";
    paths = [ (cfg.haskellPackages.ghcWithHoogle cfg.packages) ];
  };
in {
      config = mkIf cfg.enable {
        systemd.services.hoogle.serviceConfig.DynamicUser = mkForce false;
        systemd.services.hoogle.serviceConfig.User = "jupyter";
        systemd.services.hoogle.serviceConfig.Group = "users";
        systemd.services.hoogle.serviceConfig.ExecStart = mkForce ''${hoogleEnv}/bin/hoogle server --local --port ${toString cfg.port} --home ${cfg.home} --host ${cfg.host} --https --cert /etc/secrets/jupyter/mycert.pem --key /etc/secrets/jupyter/mykey.key'';
        };
}
