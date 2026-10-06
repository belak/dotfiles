# Runs Readeck behind nginx with SQLite and Pocket ID authentication.
# Setup and backups: readeck.md.
{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.belak.services.readeck;
in
{
  options.belak.services.readeck = {
    enable = lib.mkEnableOption "Readeck";

    domain = lib.mkOption {
      type = lib.types.str;
      default = "readeck.elwert.cloud";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8090;
    };
  };

  config = lib.mkIf cfg.enable {
    services.readeck = {
      enable = true;
      # Stable nixpkgs ships 0.22, before native OIDC support.
      package = pkgs.unstable.readeck;
      environmentFile = config.age.secrets.readeck-env.path;

      settings = {
        server = {
          host = "127.0.0.1";
          port = cfg.port;
          base_url = "https://${cfg.domain}";
          allowed_hosts = [ cfg.domain ];
        };

        database.source = "sqlite3:data/db.sqlite3";

        auth.oidc.providers = [
          {
            name = "Pocket ID";
            url = "https://pocket-id.elwert.cloud";
            provisioning = true;
            groups = [
              [
                "readeck:admins"
                "admin"
              ]
              [
                "readeck:users"
                "user"
              ]
            ];
          }
        ];
      };
    };

    age.secrets.readeck-env.file = ../../../../secrets/readeck-env.age;

    systemd.services.readeck.restartTriggers = [
      (builtins.hashFile "sha256" config.age.secrets.readeck-env.file)
    ];

    services.nginx.virtualHosts."${cfg.domain}" = {
      extraConfig = ''
        client_max_body_size 10m;
      '';
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString cfg.port}";
        extraConfig = ''
          proxy_set_header Host $host;
        '';
      };
    };
  };
}
