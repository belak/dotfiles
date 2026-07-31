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

    domain = lib.mkOption { default = "forgejo.elwert.cloud"; };
    databaseType = lib.mkOption { default = "sqlite3"; };
    actions = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    services.forgejo = {
      enable = true;

      package = pkgs.forgejo;

      database.type = cfg.databaseType;

      settings = {
        server = {
          ROOT_URL = "https://${cfg.domain}";
          PROTOCOL = "http+unix";
        };

        actions = {
          ENABLED = cfg.actions;
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

    # PASSWD_URI points at a stable /run/agenix path, so
    # switch-to-configuration won't restart the service on its own when
    # the secret's content changes.
    systemd.services.forgejo.restartTriggers = [ config.age.secrets.forgejo-smtp-password.file ];

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."/".proxyPass = "http://unix:${forgejoConfig.HTTP_ADDR}";
    };
  };
}
