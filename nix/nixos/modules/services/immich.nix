{ config, lib, ... }:
let
  cfg = config.belak.services.immich;
in
{
  options.belak.services.immich = {
    enable = lib.mkEnableOption "immich";

    domain = lib.mkOption { default = "photos.elwert.cloud"; };

    extraDomains = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config = lib.mkIf cfg.enable {
    services.immich = {
      enable = true;

      mediaLocation = "/mnt/immich";

      settings = {
        server.externalDomain = "https://${cfg.domain}";

        oauth = {
          enabled = true;
          #autoLaunch = true;
          issuerUrl = "https://pocket-id.elwert.cloud";
          clientId._secret = config.age.secrets.immich-oidc-client-id.path;
          clientSecret._secret = config.age.secrets.immich-oidc-client-secret.path;
        };

        notifications.smtp = {
          enabled = true;
          from = "immich@elwert.cloud";
          transport = {
            host = "smtp.fastmail.com";
            port = 465;
            secure = true;
            username = "homelab@elwert.cloud";
            password._secret = config.age.secrets.immich-smtp-password.path;
          };
        };
      };
    };

    age.secrets.immich-smtp-password = {
      file = ../../../../secrets/immich-smtp-password.age;
      owner = config.services.immich.user;
    };

    age.secrets.immich-oidc-client-id = {
      file = ../../../../secrets/immich-oidc-client-id.age;
      owner = config.services.immich.user;
    };

    age.secrets.immich-oidc-client-secret = {
      file = ../../../../secrets/immich-oidc-client-secret.age;
      owner = config.services.immich.user;
    };

    # The _secret settings point at stable /run/agenix paths, so
    # switch-to-configuration won't restart the service on its own when a
    # secret's content changes.
    #
    # The trigger hashes the file rather than pointing at it: inside a flake a
    # path is a subpath of the whole source tree, so its store hash moves on any
    # repo change, not just this secret.
    systemd.services.immich-server.restartTriggers = [
      (builtins.hashFile "sha256" config.age.secrets.immich-smtp-password.file)
      (builtins.hashFile "sha256" config.age.secrets.immich-oidc-client-id.file)
      (builtins.hashFile "sha256" config.age.secrets.immich-oidc-client-secret.file)
    ];

    services.nginx.virtualHosts."${cfg.domain}" = {
      serverAliases = cfg.extraDomains;

      extraConfig = ''
        client_max_body_size 1G;
      '';

      locations."/" = {
        proxyPass = "http://localhost:${toString config.services.immich.port}";
        proxyWebsockets = true;
      };
    };
  };
}
