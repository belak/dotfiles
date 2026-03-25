
{ config, lib, ... }:
let
  cfg = config.belak.services.immich;
in
{
  options.belak.services.immich = {
    enable = lib.mkEnableOption "immich";

    domain = lib.mkOption { default = "photos.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.immich = {
      enable = true;

      mediaLocation = "/mnt/photos";

      settings = {
        server.externalDomain = "https://${cfg.domain}";

        oauth = {
          enabled = true;
          #autoLaunch = true;
          issuerUrl = "https://pocket-id.elwert.cloud";
          clientId._secret = config.age.secrets.immich-oidc-client-id.path;
          clientSecret._secret = config.age.secrets.immich-oidc-client-secret.path;
        };

        notifications.smtp = {
          enabled = true;
          from = "immich@elwert.cloud";
          transport = {
            host = "smtp.fastmail.com";
            port = 465;
            secure = true;
            username = "homelab@elwert.cloud";
            password._secret = config.age.secrets.immich-smtp-password.path;
          };
        };
      };
    };

    age.secrets.immich-smtp-password = {
      file = ../../../../secrets/immich-smtp-password.age;
      owner = config.services.immich.user;
    };

    age.secrets.immich-oidc-client-id = {
      file = ../../../../secrets/immich-oidc-client-id.age;
      owner = config.services.immich.user;
    };

    age.secrets.immich-oidc-client-secret = {
      file = ../../../../secrets/immich-oidc-client-secret.age;
      owner = config.services.immich.user;
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."/" = {
        proxyPass = "http://localhost:${toString config.services.immich.port}";
        proxyWebsockets = true;
      };
    };
  };
}
