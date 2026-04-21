{
  config,
  lib,
  ...
}:
let
  cfg = config.belak.services.atticd;
in
{
  options.belak.services.atticd = {
    enable = lib.mkEnableOption "atticd nix binary cache";

    domain = lib.mkOption { default = "attic.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.atticd = {
      enable = true;

      environmentFile = config.age.secrets.atticd-env.path;

      settings = {
        listen = "127.0.0.1:8787";
        api-endpoint = "https://${cfg.domain}/";

        database.url = "sqlite:///var/lib/atticd/atticd.db?mode=rwc";

        storage = {
          type = "local";
          path = "/var/lib/atticd/storage";
        };

        chunking = {
          nar-size-threshold = 65536;
          min-size = 16384;
          avg-size = 65536;
          max-size = 262144;
        };
      };
    };

    # Static user so agenix can chown the env secret. Upstream module
    # defaults to DynamicUser, which doesn't exist at activation time.
    users.users.atticd = {
      isSystemUser = true;
      group = "atticd";
    };
    users.groups.atticd = { };

    systemd.services.atticd.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "atticd";
      Group = "atticd";
    };

    age.secrets.atticd-env = {
      file = ../../../../secrets/atticd-env.age;
      owner = "atticd";
    };

    belak.acme.enable = true;

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:8787";
        extraConfig = ''
          client_max_body_size 8G;
        '';
      };
    };
  };
}
