{ config, lib, ... }:
let
  cfg = config.belak.traefik;
in
{
  options.belak.traefik = {
    enable = lib.mkEnableOption "traefik";
  };

  config = lib.mkIf cfg.enable {
    services.traefik = {
      enable = true;
      environmentFiles = [ config.age.secrets.acme-cloudflare-env.path ];
      staticConfigOptions = {
        defaultEntryPoints = [ "https" ];
        log.level = "INFO";
        entryPoints = {
          http = {
            address = ":80";

            http.redirections.entryPoint = {
              to = "https";
              scheme = "https";
            };
          };

          https = {
            address = ":443";

            http.tls = {
              certResolver = "default";

              domains = [
                {
                  main = "${config.networking.hostName}.${config.networking.domain}";
                  sans = [
                    "homelab.elwert.dev"
                    "*.elwert.cloud"
                  ];
                }
              ];
            };
          };
        };

        certificatesResolvers = {
          default.acme = {
            email = "kaleb@coded.io";
            storage = "/var/lib/traefik/acme.json";

            dnsChallenge = {
              provider = "cloudflare";
            };
          };
        };

        api.dashboard = true;
      };

      dynamicConfigOptions = {
        http.middlewares.traefik-internal.ipAllowList.sourceRange = [
          # The .10 subnet is our "trusted" devices. Only computers I actively
          # use for deployment should be on there.
          "192.168.10.1/24"
        ];

        http.routers.traefik-api = {
          rule = "Host(`${config.networking.hostName}.${config.networking.domain}`)";
          service = "api@internal";
          middlewares = [ "traefik-internal" ];
        };
      };
    };

    networking.firewall.allowedTCPPorts = [
      80
      443
    ];
  };
}
