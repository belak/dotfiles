{ config, lib, ... }:
let
  cfg = config.belak.services.authelia;
  autheliaSettings = config.services.authelia.instances.main.settings;
  lldapSettings = config.services.lldap.settings;
in
{
  options.belak.services.authelia = {
    enable = lib.mkEnableOption "authelia";

    domain = lib.mkOption { default = "auth.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.authelia.instances.main = {
      enable = true;

      settings = {
        theme = "auto";

        authentication_backend.ldap = {
          address = "ldap://localhost:${toString lldapSettings.ldap_port}";
          base_dn = "dc=elwert,dc=cloud";
          additional_users_dn = "ou=people";
          implementation = "lldap";
          user = "cn=admin,ou=people,dc=elwert,dc=cloud";
        };

        access_control = {
          default_policy = "deny";
          rules = lib.mkAfter [
            {
              domain = "*.elwert.cloud";
              policy = "one_factor";
            }
          ];
        };

        storage.postgres = {
          username = "authelia-main";
          password = "dummy";
          database = "authelia-main";
          address = "unix:///var/run/postgresql";
        };

        session.cookies = [
          {
            domain = "elwert.cloud";
            authelia_url = "https://${cfg.domain}";
            name = "authelia_session";
          }
        ];

        notifier = {
          disable_startup_check = false;
          filesystem = {
            filename = "/tmp/authelia.log";
          };
        };
      };

      environmentVariables = {
        AUTHELIA_AUTHENTICATION_BACKEND_LDAP_PASSWORD_FILE =
          config.age.secrets.authelia-ldap-admin-password.path;
      };

      secrets = {
        storageEncryptionKeyFile = config.age.secrets.authelia-storage-encryption-key.path;
        jwtSecretFile = config.age.secrets.authelia-jwt-secret.path;

        # TODO: allow using RS256 and ES256
        oidcIssuerPrivateKeyFile = config.age.secrets.authelia-oidc-rs256-key.path;
      };
    };

    age.secrets.authelia-storage-encryption-key = {
      file = ../../../../secrets/authelia-storage-encryption-key.age;
      owner = "authelia-main";
    };

    age.secrets.authelia-jwt-secret = {
      file = ../../../../secrets/authelia-jwt-secret.age;
      owner = "authelia-main";
    };

    age.secrets.authelia-oidc-hmac-secret = {
      file = ../../../../secrets/authelia-oidc-hmac-secret.age;
      owner = "authelia-main";
    };

    age.secrets.authelia-oidc-rs256-key = {
      file = ../../../../secrets/authelia-oidc-rs256-key.age;
      owner = "authelia-main";
    };

    age.secrets.authelia-ldap-admin-password = {
      file = ../../../../secrets/lldap-admin-password.age;
      owner = "authelia-main";
    };

    users.users.authelia-main = {
      group = "authelia-main";
      isSystemUser = true;
    };

    users.groups.authelia-main = { };

    services.postgresql = {
      ensureDatabases = [ "authelia-main" ];
      ensureUsers = [
        {
          name = "authelia-main";
          ensureDBOwnership = true;
        }
      ];
    };

    belak.acme.enable = true;

    services.nginx.virtualHosts."${cfg.domain}" = {
      useACMEHost = "primary";
      forceSSL = true;

      # TODO: make this use a unix socket
      locations."/".proxyPass = "http://localhost:9091";
    };
  };
}
