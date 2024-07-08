{ config, lib, ... }:
let
  cfg = config.belak.gitea;
  giteaConfig = config.services.gitea.settings.server;
in
{
  options.belak.gitea = {
    enable = lib.mkEnableOption "gitea";

    domain = lib.mkOption { default = "gitea.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.gitea = {
      enable = true;

      database.type = "postgres";

      settings.server = {
        ROOT_URL = "https://${cfg.domain}";
        HTTP_ADDR = "127.0.0.1";
      };
    };

    services.traefik.dynamicConfigOptions = {
      http.services.gitea.loadBalancer.servers = [ "http://localhost:${giteaConfig.HTTP_PORT}" ];

      http.routers.gitea = {
        rule = "Host(`${cfg.domain}`)";
        service = "gitea@file";
      };
    };
  };
}
