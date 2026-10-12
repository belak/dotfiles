{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.belak.services.caddy;

  # Plain HTTP sites need an explicit scheme so Caddy skips automatic HTTPS.
  address = host: if cfg.tls then host else "http://${host}";
in
{
  options = {
    belak.services.caddy = {
      enable = lib.mkEnableOption "caddy";
      package = lib.mkPackageOption pkgs.unstable "caddy" { };

      tls = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };

      virtualHosts = lib.mkOption {
        type = lib.types.attrsOf (
          lib.types.submodule (
            { name, ... }:
            {
              options = {
                hostName = lib.mkOption {
                  type = lib.types.str;
                  default = name;
                };

                backend = lib.mkOption {
                  type = lib.types.nullOr lib.types.str;
                  default = null;
                };

                extraHosts = lib.mkOption {
                  type = lib.types.listOf lib.types.str;
                  default = [ ];
                };

                useACMEHost = lib.mkOption {
                  type = lib.types.str;
                  default = "primary";
                };

                extraConfig = lib.mkOption {
                  type = lib.types.lines;
                  default = "";
                };
              };
            }
          )
        );
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
          hostName = address value.hostName;
          serverAliases = map address value.extraHosts;
          useACMEHost = if cfg.tls then value.useACMEHost else null;
          # Access logs go to the journal and include the authenticated user.
          # They need their own output: the NixOS module sets the default
          # logger to ERROR, which would drop them. The clacks headers are only
          # added at the edge: backend responses pass through it, and adding
          # them there too would duplicate them.
          extraConfig = ''
            log {
              output stderr
            }
            ${lib.optionalString cfg.tls ''
              header +X-Clacks-Overhead "GNU Douglas Adams"
              header +X-Clacks-Overhead "GNU Robert Asprin"
            ''}
            ${lib.optionalString (value.backend != null) "reverse_proxy ${value.backend}"}

            ${value.extraConfig}
          '';
        };
      }) cfg.virtualHosts;
    };

    networking.firewall.allowedTCPPorts = [
      80
    ]
    ++ lib.optional cfg.tls 443;
  };
}
