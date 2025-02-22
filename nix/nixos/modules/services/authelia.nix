{ config, lib, ... }:
let
  cfg = config.belak.services.authelia;
  autheliaSettings = config.services.authelia.instances.main.settings;
in
{
  options.belak.services.authelia = {
    enable = lib.mkEnableOption "authelia";

    domain = lib.mkOption { default = "authelia.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.authelia.instances.main = {
      enable = true;

      settings = {
        theme = "auto";
      };

      secrets = {
        storageEncryptionKeyFile = config.age.secrets.authelia-storage-encryption-key.path;
        jwtSecretFile = config.age.secrets.authelia-jwt-secret.path;
      };
    };

    age.secrets.authelia-storage-encryption-key.file = ../../../secrets/authelia-storage-encryption-key.age;
    age.secrets.authelia-jwt-secret.file = ../../../secrets/authelia-jwt-secret.age;

    services.postgresql = {
      ensureDatabases = [ "authelia" ];
      ensureUsers = [
        {
          name = "authelia";
          ensureDBOwnership = true;
        }
      ];
    };

    belak.acme.enable = true;

    services.nginx.virtualHosts."${cfg.domain}" = {
      useACMEHost = "primary";
      forceSSL = true;

      locations."/".proxyPass = "http://localhost:9091";
    };
  };
}
