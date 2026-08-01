{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.services.kavita;
in
{
  options.belak.services.kavita = {
    enable = lib.mkEnableOption "kavita";

    domain = lib.mkOption { default = "kavita.elwert.cloud"; };

    port = lib.mkOption {
      type = lib.types.port;
      default = 5000;
    };

    oidc = {
      enable = lib.mkEnableOption "OIDC authentication via pocket-id";

      authority = lib.mkOption {
        type = lib.types.str;
        default = "https://pocket-id.elwert.cloud";
      };

      clientId = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "OIDC client ID issued by pocket-id.";
      };
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        services.kavita = {
          enable = true;
          tokenKeyFile = config.age.secrets.kavita-token-key.path;
          settings = {
            Port = cfg.port;
            # Only listen on loopback; nginx handles the public interface.
            IpAddresses = "127.0.0.1,::1";
          };
        };

        services.nginx.virtualHosts."${cfg.domain}" = {
          locations."/" = {
            # We can't use recommendedProxySettings (see the nginx module), but
            # nginx defaults Host to $proxy_host, so it has to be set explicitly.
            # The X-Forwarded-* headers from caddy pass through untouched.
            extraConfig = ''
              proxy_set_header Host $host;
            '';

            proxyPass = "http://127.0.0.1:${toString cfg.port}";
            proxyWebsockets = true;
          };
        };

        age.secrets.kavita-token-key = {
          file = ../../../../secrets/kavita-token-key.age;
          owner = "kavita";
        };
      }

      (lib.mkIf cfg.oidc.enable {
        # Kavita overwrites appsettings.json from the Nix store on every start,
        # so OIDC has to be declared here rather than in the web UI. The real
        # client secret must not land in the world-readable store, so we only
        # write an @OIDC_SECRET@ placeholder and swap it for the age-decrypted
        # value at preStart (same trick the module uses for the TokenKey).
        services.kavita.settings.OpenIdConnectSettings = {
          Authority = cfg.oidc.authority;
          ClientId = cfg.oidc.clientId;
          Secret = "@OIDC_SECRET@";
          CustomScopes = [ ];
          Enabled = true;
        };

        age.secrets.kavita-oidc-client-secret = {
          file = ../../../../secrets/kavita-oidc-client-secret.age;
          owner = "kavita";
        };

        # Matches the module's handling of the TokenKey: systemd stages the
        # secret in the unit's private credentials dir (LoadCredential list
        # directives concatenate, so this appends to the upstream token entry).
        systemd.services.kavita.serviceConfig.LoadCredential = [
          "oidc-secret:${config.age.secrets.kavita-oidc-client-secret.path}"
        ];

        systemd.services.kavita.preStart = lib.mkAfter ''
          ${pkgs.replace-secret}/bin/replace-secret '@OIDC_SECRET@' \
            "''${CREDENTIALS_DIRECTORY}/oidc-secret" \
            '${config.services.kavita.dataDir}/config/appsettings.json'
        '';
      })
    ]
  );
}
