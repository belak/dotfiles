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
        database_url = "postgres:///lldap";
        ldap_base_dn = "dc=elwert,dc=cloud";
        ldap_host = "127.0.0.1";
        http_host = "127.0.0.1";
        #force_ldap_user_pass_reset = true;
      };

      environment = {
        LLDAP_JWT_SECRET_FILE = config.age.secrets.lldap-jwt-secret.path;
        LLDAP_LDAP_USER_PASS_FILE = config.age.secrets.lldap-admin-password.path;
      };
    };

    users.users.lldap = {
      group = "lldap";
      isSystemUser = true;
    };

    users.groups.lldap = { };

    services.postgresql = {
      ensureDatabases = [ "lldap" ];
      ensureUsers = [
        {
          name = "lldap";
          ensureDBOwnership = true;
        }
      ];
    };

    age.secrets.lldap-jwt-secret = {
      file = ../../../../secrets/lldap-jwt-secret.age;
      owner = "lldap";
    };

    age.secrets.lldap-admin-password = {
      file = ../../../../secrets/lldap-admin-password.age;
      owner = "lldap";
    };

    belak.acme.enable = true;

    services.nginx.virtualHosts."${cfg.domain}" = {
      useACMEHost = "primary";
      forceSSL = true;

      locations."/".proxyPass = "http://localhost:${toString lldapSettings.http_port}";
    };
  };
}
