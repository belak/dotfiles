{ config, lib, ... }:
let
  cfg = config.belak.services.miniflux;
in
{
  options.belak.services.miniflux = {
    enable = lib.mkEnableOption "miniflux";
    domain = lib.mkOption { default = "miniflux.elwert.cloud"; };
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
        OAUTH2_OIDC_DISCOVERY_ENDPOINT = "https://pocket-id.elwert.cloud";
        OAUTH2_USER_CREATION = 1;
        #DISABLE_LOCAL_AUTH = 1;
      };
      adminCredentialsFile = config.age.secrets.miniflux-admin-credentials.path;
    };

    users.users.miniflux = {
      group = "miniflux";
      isSystemUser = true;
    };

    users.groups.miniflux = {
      members = [ "nginx" ];
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
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

    # The _FILE settings point at stable /run/agenix paths, so
    # switch-to-configuration won't restart the service on its own when a
    # secret's content changes.
    #
    # The trigger hashes the file rather than pointing at it: inside a flake a
    # path is a subpath of the whole source tree, so its store hash moves on any
    # repo change, not just this secret.
    systemd.services.miniflux.restartTriggers = [
      (builtins.hashFile "sha256" config.age.secrets.miniflux-admin-credentials.file)
      (builtins.hashFile "sha256" config.age.secrets.miniflux-oidc-client-id.file)
      (builtins.hashFile "sha256" config.age.secrets.miniflux-oidc-client-secret.file)
    ];
  };
}
