{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.belak.services.forgejo;
  forgejoConfig = config.services.forgejo.settings.server;
in
{
  options.belak.services.forgejo = {
    enable = lib.mkEnableOption "forgejo";

    domain = lib.mkOption { default = "git.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.forgejo = {
      enable = true;

      package = pkgs.forgejo;

      database.type = "postgres";

      settings = {
        server = {
          ROOT_URL = "https://${cfg.domain}";
          PROTOCOL = "http+unix";
        };

        service = {
          DISABLE_REGISTRATION = true;
        };

        oauth2_client = {
          ENABLE_AUTO_REGISTRATION = true;
        };

        mailer = {
          ENABLED = true;
          PROTOCOL = "smtps";
          SMTP_ADDR = "smtp.fastmail.com";
          SMTP_PORT = 465;
          USER = "homelab@elwert.cloud";
          FROM = "forgejo@elwert.cloud";
          PASSWD_URI = "file:${config.age.secrets.forgejo-smtp-password.path}";
        };
      };
    };

    age.secrets.forgejo-smtp-password = {
      file = ../../../../secrets/forgejo-smtp-password.age;
      owner = config.services.forgejo.user;
    };

    belak.acme.enable = true;

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."/".proxyPass = "http://unix:${forgejoConfig.HTTP_ADDR}";
    };
  };
}
