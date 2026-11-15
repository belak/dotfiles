{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.services.oauth2-proxy;

  mkService = name: inst: {
    "oauth2-proxy-${name}" = {
      description = "oauth2-proxy for ${name}";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        ExecStart = lib.escapeShellArgs (
          [
            (lib.getExe cfg.package)
            "--http-address=127.0.0.1:${toString inst.port}"
            "--provider=oidc"
            "--oidc-issuer-url=${inst.issuerUrl}"
            "--redirect-url=https://${inst.domain}/oauth2/callback"
            # Auth-only: Caddy proxies to the backend after a 2xx.
            "--upstream=static://202"
            # Pocket ID decides who may use the client.
            "--email-domain=*"
            "--cookie-secure=true"
            "--reverse-proxy=true"
            "--trusted-proxy-ip=127.0.0.1/32"
            "--trusted-proxy-ip=::1/128"
            "--set-xauthrequest=true"
          ]
          ++ inst.extraArgs
        );
        EnvironmentFile = inst.envFile;
        Restart = "always";
        RestartSec = "10s";
        DynamicUser = true;
      };
    };
  };

  # route runs before the vhost's top-level reverse_proxy to the backend.
  mkVirtualHost = _: inst: {
    ${inst.domain}.extraConfig = ''
      route {
        reverse_proxy /oauth2/* 127.0.0.1:${toString inst.port} {
          header_up X-Real-IP {remote_host}
          header_up X-Forwarded-Uri {uri}
        }
        forward_auth 127.0.0.1:${toString inst.port} {
          uri /oauth2/auth
          header_up X-Real-IP {remote_host}
          copy_headers X-Auth-Request-User>X-Forwarded-User X-Auth-Request-Email>X-Forwarded-Email
          @unauthorized status 401
          handle_response @unauthorized {
            redir * /oauth2/sign_in?rd={uri}
          }
        }
      }
    '';
  };

  ports = lib.mapAttrsToList (_: inst: inst.port) cfg.instances;
in
{
  options.belak.services.oauth2-proxy = {
    package = lib.mkPackageOption pkgs "oauth2-proxy" { };

    instances = lib.mkOption {
      default = { };
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            domain = lib.mkOption {
              type = lib.types.str;
              description = "Caddy virtual host to protect.";
            };

            port = lib.mkOption {
              type = lib.types.port;
              description = "Loopback port for this instance.";
            };

            envFile = lib.mkOption {
              type = lib.types.str;
              description = "Sets OAUTH2_PROXY_CLIENT_ID, _CLIENT_SECRET and _COOKIE_SECRET.";
            };

            issuerUrl = lib.mkOption {
              type = lib.types.str;
              default = "https://pocket-id.elwert.cloud";
            };

            extraArgs = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
            };
          };
        }
      );
    };
  };

  config = lib.mkIf (cfg.instances != { }) {
    assertions = [
      {
        assertion = lib.allUnique ports;
        message = "belak.services.oauth2-proxy: instances must use distinct ports.";
      }
    ];

    systemd.services = lib.mkMerge (lib.mapAttrsToList mkService cfg.instances);

    belak.services.caddy.virtualHosts = lib.mkMerge (lib.mapAttrsToList mkVirtualHost cfg.instances);
  };
}
