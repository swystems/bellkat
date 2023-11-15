{ config, pkgs, lib, modulesPath, bellkat, ... }:
let
  matplotlibLatex = pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-basic type1cm cm-super underscore dvipng;
  };

  dontCheckBellKAT = final: prev: {
    haskellPackages = prev.haskellPackages.extend (hself: hsuper: {
      bellkat = prev.haskell.lib.dontCheck hsuper.bellkat;
    });
  };

  ihaskell-env = (pkgs.haskellPackages.ghcWithPackages.override {
    postBuild = ''
      wrapProgram $out/bin/ihaskell --prefix PATH : $out/bin
    '';
  }) (ps: [
    ps.ihaskell
    ps.ihaskell-blaze
    ps.bellkat
  ]);

  ihaskell-kernel = pkgs.runCommand "ihaskell-kernel" {
    buildInputs = [ ihaskell-env pkgs.python310Packages.notebook ];
  } ''
    HOME=$(pwd) ihaskell install --prefix=$(pwd) --ghclib=$(${ihaskell-env}/bin/ghc --print-libdir)

    mv share/jupyter/kernels/haskell/kernel.json share/jupyter/kernels/haskell/kernel.old.json 
    ${pkgs.jq}/bin/jq '.env = {"PATH": "${ihaskell-env}/bin"}' share/jupyter/kernels/haskell/kernel.old.json > share/jupyter/kernels/haskell/kernel.json

    mkdir $out
    mv share/jupyter/kernels $out
  '';
  pythonKernels = pkgs.jupyter-kernel.create {
    definitions = {
      python3 = let
        env = pkgs.python310.buildEnv.override {
          extraLibs = with pkgs.python310Packages; [
            ipykernel
            matplotlib
            networkx
            pydot
          ];
          makeWrapperArgs =
            [ "--prefix" "PATH" ":" (lib.makeBinPath [ matplotlibLatex ]) ];
        };
      in {
        displayName = "Python 3 for machine learning";
        argv = [
          "${env.interpreter}"
          "-m"
          "ipykernel_launcher"
          "-f"
          "{connection_file}"
        ];
        language = "python";
        logo32 = null;
        logo64 = null;
      };
    };
  };
  kernels = pkgs.symlinkJoin {
    name = "kernels";
    paths = [ pythonKernels ihaskell-kernel ];
  };
in {
  imports =
    [ (modulesPath + "/virtualisation/amazon-image.nix") ./hoogle_tls.nix ];

  nixpkgs.overlays =
    [ bellkat.overlays.ihaskell-diagrams-fix bellkat.overlays.default dontCheckBellKAT ];

  nix.settings.trusted-users = [ "pschuprikov" ];
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  users.users.pschuprikov = {
    isNormalUser = true;

    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFf6rKmu0D5TI7WLn/OTU9A6s2+BQyetqY1l1/K/SLxx pschuprikov@usimachine"
    ];
    extraGroups = [ "wheel" ];
  };

  users.mutableUsers = false;

  security.sudo.extraRules = [{
    users = [ "pschuprikov" ];
    groups = [ "ALL" ];
    commands = [{
      command = "ALL";
      options = [ "NOPASSWD" ];
    }];
  }];

  networking.firewall.allowedTCPPorts =
    [ config.services.jupyter.port config.services.hoogle.port ] ++ config.services.openssh.ports;

  users.groups.jupyter = { };
  users.users.jupyter.group = "jupyter";

  services.hoogle.enable = true;
  services.hoogle.packages = ps: [ ps.bellkat ];
  services.hoogle.host = "0.0.0.0";

  services.jupyter.package =
    pkgs.python310.withPackages (ps: [ ps.jupyter ps.notebook ]);
  services.jupyter.enable = true;
  services.jupyter.ip = "*";
  services.jupyter.port = 8888;
  services.jupyter.group = "users";
  services.jupyter.user = "jupyter";
  services.jupyter.notebookDir = "~/notebooks";
  services.jupyter.password =
    "'argon2:$argon2id$v=19$m=10240,t=10,p=8$DL6exbRkw6urXsqEq6YDgQ$P24kuNWNS02FMuh1THSBju6p6q1c0bynnyOUB/upbB8'";

  systemd.services.jupyter.environment.JUPYTER_PATH =
    pkgs.lib.mkForce (toString kernels);

  systemd.services.amazon-init.enable = false;

  services.jupyter.notebookConfig = ''
    c.NotebookApp.certfile = '/etc/secrets/jupyter/mycert.pem'
    c.NotebookApp.keyfile = '/etc/secrets/jupyter/mykey.key'
  '';

  environment.systemPackages = with pkgs; [ git vim tmux ];

  system.stateVersion = "22.05";
}
