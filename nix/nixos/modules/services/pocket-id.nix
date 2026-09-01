{ config, lib, ... }:
let
  cfg = config.belak.services.pocket-id;
in
{
  options.belak.services.pocket-id = {
    enable = lib.mkEnableOption "pocketpid";

    domain = lib.mkOption { default = "pocket-id.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.pocket-id = {
      enable = true;

      settings = {
        TRUST_PROXY = true;
        APP_URL = "https://${cfg.domain}";
        ENCRYPTION_KEY_FILE = config.age.secrets.pocket-id-encryption-key.path;
        UI_CONFIG_DISABLED = true;
        APP_NAME = "Elwert Auth";

        # Fastmail settings
        SMTP_HOST = "smtp.fastmail.com";
        SMTP_PORT = 465;
        SMTP_TLS = "tls";

        SMTP_FROM = "pocket-id@elwert.cloud";
        SMTP_USER = "homelab@elwert.cloud";
        SMTP_PASSWORD_FILE = config.age.secrets.pocket-id-smtp-password.path;

        EMAILS_VERIFIED = true;
        EMAIL_ONE_TIME_ACCESS_AS_ADMIN_ENABLED = true;
        # TODO: switch to UNIX_SOCKET
      };
    };

    age.secrets.pocket-id-encryption-key = {
      file = ../../../../secrets/pocket-id-encryption-key.age;
      owner = config.services.pocket-id.user;
    };

    age.secrets.pocket-id-smtp-password = {
      file = ../../../../secrets/pocket-id-smtp-password.age;
      owner = config.services.pocket-id.user;
    };

    # The _FILE settings point at stable /run/agenix paths, so
    # switch-to-configuration won't restart the service on its own when a
    # secret's content changes.
    #
    # The trigger hashes the file rather than pointing at it: inside a flake a
    # path is a subpath of the whole source tree, so its store hash moves on any
    # repo change, not just this secret.
    systemd.services.pocket-id.restartTriggers = [
      (builtins.hashFile "sha256" config.age.secrets.pocket-id-encryption-key.file)
      (builtins.hashFile "sha256" config.age.secrets.pocket-id-smtp-password.file)
    ];

    services.nginx.virtualHosts."${cfg.domain}" = {
      # TODO: make this use a unix socket
      locations."/".proxyPass = "http://localhost:1411";
    };
  };
}
