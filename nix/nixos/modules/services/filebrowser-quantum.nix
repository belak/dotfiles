# Runs FileBrowser Quantum behind nginx on a unix socket with Pocket ID login.
# The Pocket ID client's callback URL is https://<domain>/api/auth/oidc/callback.
# filebrowser-quantum-env.age holds FILEBROWSER_OIDC_CLIENT_ID and
# FILEBROWSER_OIDC_CLIENT_SECRET.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.services.filebrowser-quantum;
  format = pkgs.formats.yaml { };
  configFile = format.generate "filebrowser-quantum.yaml" cfg.settings;

  name = "filebrowser-quantum";
  socket = "/run/${name}/${name}.sock";
  envSecret = config.age.secrets.filebrowser-quantum-env;

  sourcePaths = map (source: source.path) cfg.settings.server.sources;
in
{
  options.belak.services.filebrowser-quantum = {
    enable = lib.mkEnableOption "FileBrowser Quantum";

    package = lib.mkPackageOption pkgs.my "filebrowser-quantum" { };

    domain = lib.mkOption {
      type = lib.types.str;
      default = "files.elwert.cloud";
    };

    maxUploadSize = lib.mkOption {
      type = lib.types.str;
      default = "10G";
    };

    oidc = {
      issuerUrl = lib.mkOption {
        type = lib.types.str;
        default = "https://pocket-id.elwert.cloud";
      };

      userGroup = lib.mkOption {
        type = lib.types.str;
        default = "filebrowser:users";
      };

      adminGroup = lib.mkOption {
        type = lib.types.str;
        default = "filebrowser:admins";
      };
    };

    settings = lib.mkOption {
      type = format.type;
      default = { };
    };
  };

  config = lib.mkIf cfg.enable {
    belak.services.filebrowser-quantum.settings = {
      http = {
        socket = socket;
        trustProxyHeaders = true;
        externalUrl = "https://${cfg.domain}";
      };

      server = {
        database.path = "/var/lib/${name}/filebrowser.sqlite";
        cacheDir = "/var/cache/${name}";
        disableUpdateCheck = true;
      };

      auth.methods = {
        password.enabled = false;
        oidc = {
          enabled = true;
          issuerUrl = cfg.oidc.issuerUrl;
          # Pocket ID only returns the groups claim when asked for it.
          scopes = "openid email profile groups";
          adminGroup = cfg.oidc.adminGroup;
          userGroups = [
            cfg.oidc.userGroup
            cfg.oidc.adminGroup
          ];
        };
      };

      userDefaults.account.permissions.share = true;
    };

    users.users.${name} = {
      group = name;
      isSystemUser = true;
    };

    # nginx needs the group to connect to the socket.
    users.groups.${name}.members = [ "nginx" ];

    age.secrets.filebrowser-quantum-env.file = ../../../../secrets/filebrowser-quantum-env.age;

    systemd.services.${name} = {
      description = "FileBrowser Quantum";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];

      # The app finds ffmpeg and ffprobe on PATH for video thumbnails.
      path = [ pkgs.ffmpeg-headless ];

      # Starting before a source is mounted would index the empty mountpoint.
      unitConfig.RequiresMountsFor = sourcePaths;

      # The env file holds the OIDC client ID and secret. It is read by
      # systemd at start, so a change to the secret does not restart the
      # service on its own. The trigger hashes the file rather than pointing
      # at it: inside a flake a path is a subpath of the whole source tree,
      # so its store hash moves on any repo change, not just this secret.
      restartTriggers = [
        (builtins.hashFile "sha256" envSecret.file)
      ];

      serviceConfig = {
        ExecStart = "${lib.getExe cfg.package} --config ${configFile}";
        EnvironmentFile = envSecret.path;
        # Startup fails when Pocket ID is unreachable, so retry slowly instead
        # of hitting the start limit.
        Restart = "on-failure";
        RestartSec = "10s";

        User = name;
        Group = name;
        # Group-writable so nginx can connect to the socket.
        UMask = "0007";
        StateDirectory = name;
        CacheDirectory = name;
        RuntimeDirectory = name;
        RuntimeDirectoryMode = "0750";
        WorkingDirectory = "/var/lib/${name}";

        # Only the sources are writable; everything else is read-only.
        ProtectSystem = "strict";
        ReadWritePaths = sourcePaths;
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        NoNewPrivileges = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        ProtectClock = true;
        ProtectHostname = true;
        ProtectProc = "invisible";
        ProcSubset = "pid";
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        LockPersonality = true;
        SystemCallArchitectures = "native";
        CapabilityBoundingSet = "";
      };
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      extraConfig = ''
        client_max_body_size ${cfg.maxUploadSize};
      '';

      locations."/" = {
        proxyPass = "http://unix:${socket}";

        # We can't use recommendedProxySettings (see the nginx module), so
        # Host is set explicitly. The X-Forwarded-* headers from caddy pass
        # through untouched, which the app needs (trustProxyHeaders) to build
        # the OIDC callback and share URLs. Buffering is off so uploads and
        # downloads stream and the SSE connection isn't held back.
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_http_version 1.1;
          proxy_buffering off;
          proxy_request_buffering off;
          proxy_read_timeout 600s;
        '';
      };
    };
  };
}
