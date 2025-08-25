{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.belak.services.caddy;
in
{
  options = {
    belak.services.caddy = {
      enable = lib.mkEnableOption "caddy";
      package = lib.mkPackageOption pkgs.unstable "caddy" { };

      virtualHosts = lib.mkOption {
        type = lib.types.attrsOf (lib.types.submodule ({ name, ... }: {
          options = {
            hostName = lib.mkOption {
              type = lib.types.str;
              default = name;
            };

            backend = lib.mkOption {
              type = lib.types.str;
            };

            extraHosts = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [];
            };

            useACMEHost = lib.mkOption {
              type = lib.types.str;
              default = "primary";
            };
          };
        }));
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.caddy = {
      enable = true;
      package = cfg.package;

      globalConfig = ''
        servers {
          trusted_proxies static private_ranges
        }
      '';

      virtualHosts = lib.concatMapAttrs (name: value: {
        ${value.hostName} = {
          serverAliases = value.extraHosts;
          useACMEHost = value.useACMEHost;
          extraConfig = ''
            header +X-Clacks-Overhead "GNU Douglas Adams"
            header +X-Clacks-Overhead "GNU Robert Asprin"

            reverse_proxy ${value.backend}
          '';
        };
      }) cfg.virtualHosts;
    };

    networking.firewall.allowedTCPPorts = [
      80
      443
    ];
  };
}
