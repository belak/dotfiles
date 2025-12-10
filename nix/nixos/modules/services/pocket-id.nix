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
        KEYS_STORAGE = "database";
        UI_CONFIG_DISABLED = true;
        APP_NAME = "Elwert Auth";
        # TODO: switch to UNIX_SOCKET
      };
    };

    age.secrets.pocket-id-encryption-key = {
      file = ../../../../secrets/pocket-id-encryption-key.age;
      owner = config.services.pocket-id.user;
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      # TODO: make this use a unix socket
      locations."/".proxyPass = "http://localhost:1411";
    };
  };
}
