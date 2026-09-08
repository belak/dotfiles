{ config, lib, pkgs, ... }:

let
  cfg = config.belak.services.taskchampion;
in
{
  options.belak.services.taskchampion = {
    enable = lib.mkEnableOption "taskchampion sync server";
    domain = lib.mkOption {
      type = lib.types.str;
      default = "taskwarrior.elwert.cloud";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 10222;
    };
  };

  config = lib.mkIf cfg.enable {
    services.taskchampion-sync-server = {
      enable = true;
      port = cfg.port;
      # We intentionally do not use allowClientIds so we don't leak it in the public NixOS config.
      # Instead, we rely on Nginx basicAuth to restrict access to the server.
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      basicAuthFile = config.age.secrets.taskchampion-basic-auth.path;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString cfg.port}";
      };
    };

    age.secrets.taskchampion-basic-auth = {
      file = ../../../../secrets/taskchampion-basic-auth.age;
      owner = "nginx";
    };

    systemd.services.nginx.restartTriggers = [
      (builtins.hashFile "sha256" config.age.secrets.taskchampion-basic-auth.file)
    ];
  };
}
