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
        PROTOCOL = "http+unix";
      };
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      useACMEHost = "primary";
      forceSSL = true;

      locations."/".proxyPass = "http://unix:${giteaConfig.HTTP_ADDR}";
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
