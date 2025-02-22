{ config, lib, ... }:
let
  cfg = config.belak.services.lldap;
  lldapSettings = config.services.lldap.settings;
in
{
  options.belak.services.lldap = {
    enable = lib.mkEnableOption "lldap";

    domain = lib.mkOption { default = "lldap.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.lldap = {
      enable = true;

      settings = {
        http_url = "https://${cfg.domain}";
        database_url = "postgres://lldap@localhost/lldap";
        ldap_base_dn = "dc=elwert,dc=cloud";
        ldap_host = "127.0.0.1";
        http_host = "127.0.0.1";
      };
    };

    services.postgresql = {
      ensureDatabases = [ "lldap" ];
      ensureUsers = [
        {
          name = "lldap";
          ensureDBOwnership = true;
        }
      ];
    };

    belak.acme.enable = true;

    services.nginx.virtualHosts."${cfg.domain}" = {
      useACMEHost = "primary";
      forceSSL = true;

      locations."/".proxyPass = "http://localhost:${lldapSettings.http_port}";
    };
  };
}
