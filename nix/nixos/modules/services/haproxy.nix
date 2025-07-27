{ config, lib, ... }:
let
  cfg = config.belak.services.haproxy;
in
{
  options.belak.services.haproxy = {
    enable = lib.mkEnableOption "haproxy";
    acmeCerts = lib.mkOption {
      type = lib.types.listOf lib.types.str;
    };
    backends = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
            options = {
              name = lib.mkOption {
                type = lib.types.str;
                default = name;
              };

              mode = lib.mkOption {
                type = lib.types.enum [
                  "http"
                  "tcp"
                ];
                default = "http";
              };

              servers = lib.mkOption {
                type = with lib.types; attrsOf str;
              };

              matchers = lib.mkOption {
                type = with lib.types; listOf str;
              };
            };
          }
        )
      );
    };
  };

  config = lib.mkIf cfg.enable {
    services.haproxy =
      let
        crtStore = map (
          f:
          let
            certDir = config.security.acme.certs.${f}.directory;
          in
          "load crt \"${certDir}/cert.pem\" key \"${certDir}/key.pem\" alias ${f}"
        ) cfg.acmeCerts;

        crtBind = map (f: "crt @/${f}") cfg.acmeCerts;

        clacksHeaders = [
          ''http-response add-header X-Clacks-Overhead "GNU Douglas Adams"''
          ''http-response add-header X-Clacks-Overhead "GNU Robert Asprin"''
        ];

        backendMatchers = lib.flatten (
          lib.mapAttrsToList (
            name: backend: (map (matcher: "use_backend ${name} ${matcher}") backend.matchers)
          ) cfg.backends
        );

        backends = lib.mapAttrsToList (
          name: backend:
          let
            servers = builtins.concatStringsSep "\n  " (
              lib.mapAttrsToList (serverName: server: "server ${serverName} ${server}") backend.servers
            );
          in
          ''
            backend ${backend.name}
              mode ${backend.mode}
              ${servers}
          ''
        ) cfg.backends;

      in
      {
        enable = true;

        config = ''
          global
            log /dev/log local0 info

          defaults
            log global

            timeout connect 10s
            timeout client 30s
            timeout server 30s
            timeout tunnel 15m

          crt-store
            ${builtins.concatStringsSep "\n  " crtStore}

          frontend http
            mode http
            bind :80

            option httplog

            ${builtins.concatStringsSep "\n  " clacksHeaders}

            # Redirect to https
            http-request redirect scheme https unless { ssl_fc }

          frontend https
            mode http
            bind :443 ssl ${builtins.concatStringsSep " " crtBind}

            option httplog
            option forwardfor

            ${builtins.concatStringsSep "\n  " clacksHeaders}

            ${builtins.concatStringsSep "\n  " backendMatchers}

          ${builtins.concatStringsSep "\n" backends}
        '';
      };

    # We need to make sure our tls certs are available before starting haproxy,
    # otherwise it may fail to start.
    systemd.services.haproxy = {
      after = (map (f: "acme-selfsigned-${f}.target") cfg.acmeCerts);
      requires = (map (f: "acme-finished-${f}.target") cfg.acmeCerts);
    };

    networking.firewall.allowedTCPPorts = [
      80
      443
    ];
  };
}
