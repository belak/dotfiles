{ config, lib, ... }:
let
  cfg = config.belak.services.miniflux;
in
{
  options.belak.services.miniflux = {
    enable = lib.mkEnableOption "miniflux";
    domain = lib.mkOption { default = "rss.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.miniflux = {
      enable = true;
      config = {
        LISTEN_ADDR = "/run/miniflux/miniflux.sock";
        OAUTH2_PROVIDER = "oidc";
        OAUTH2_CLIENT_ID_FILE = config.age.secrets.miniflux-oidc-client-id.path;
        OAUTH2_CLIENT_SECRET_FILE = config.age.secrets.miniflux-oidc-client-secret.path;
        OAUTH2_REDIRECT_URL = "https://${cfg.domain}/oauth2/oidc/callback";
        OAUTH2_OIDC_DISCOVERY_ENDPOINT = "https://auth.elwert.cloud";
        OAUTH2_USER_CREATION = 1;
        DISABLE_LOCAL_AUTH = 1;
      };
      adminCredentialsFile = config.age.secrets.miniflux-admin-credentials.path;
    };

    belak.acme.enable = true;

    users.users.miniflux = {
      group = "miniflux";
      isSystemUser = true;
    };

    users.groups.miniflux = {
      members = [ "nginx" ];
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      useACMEHost = "primary";
      forceSSL = true;
      locations."/".proxyPass = "http://unix:/run/miniflux/miniflux.sock";
    };

    age.secrets.miniflux-admin-credentials = {
      file = ../../../../secrets/miniflux-admin-credentials.age;
      owner = "miniflux";
    };

    age.secrets.miniflux-oidc-client-id = {
      file = ../../../../secrets/miniflux-oidc-client-id.age;
      owner = "miniflux";
    };

    age.secrets.miniflux-oidc-client-secret = {
      file = ../../../../secrets/miniflux-oidc-client-secret.age;
      owner = "miniflux";
    };
  };
}
