{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.services.inkheart;

  name = "inkheart";
  stateDir = "/var/lib/${name}";
  cacheDir = "/var/cache/${name}";

  toEnv = v: if builtins.isBool v then lib.boolToString v else toString v;
in
{
  options.belak.services.inkheart = {
    enable = lib.mkEnableOption "Inkheart";

    package = lib.mkPackageOption pkgs.my "inkheart" { };

    domain = lib.mkOption {
      type = lib.types.str;
      default = "inkheart.elwert.cloud";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8090;
    };

    mediaDir = lib.mkOption {
      type = lib.types.str;
      description = "Directory holding the PDF library. Read-only is fine.";
    };

    oauth2Proxy = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Require a Pocket ID login via oauth2-proxy.";
      };

      port = lib.mkOption {
        type = lib.types.port;
        default = 8091;
      };
    };

    settings = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.oneOf [
          lib.types.str
          lib.types.int
          lib.types.bool
        ]
      );
      default = { };
      description = "Environment variables; see the upstream README.";
    };
  };

  config = lib.mkIf cfg.enable {
    belak.services.inkheart.settings = {
      MEDIA_DIR = lib.mkDefault cfg.mediaDir;
      IMAGE_DIR = lib.mkDefault "${cacheDir}/covers";
      THUMBNAIL_DIR = lib.mkDefault "${cacheDir}/thumbnails";
      CONFIG_DIR = lib.mkDefault stateDir;
      CONFIG_PATH = lib.mkDefault "${stateDir}/inkheart.toml";
      FIREBASE_CONFIG_PATH = lib.mkDefault "${stateDir}/firebase.json";
      BIND_ADDR = lib.mkDefault "127.0.0.1";
      BIND_PORT = lib.mkDefault cfg.port;
      SCAN_INTERVAL = lib.mkDefault 600;
      TELEMETRY_ENABLED = lib.mkDefault false;
      TEXT_INDEX_ENABLED = lib.mkDefault true;
      INDEX_SNAPSHOT_ENABLED = lib.mkDefault true;
    };

    users.users.${name} = {
      group = name;
      isSystemUser = true;
    };

    users.groups.${name} = { };

    systemd.services.${name} = {
      description = "Inkheart";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];

      # Starting before the library is mounted would index an empty dir.
      unitConfig.RequiresMountsFor = [ cfg.mediaDir ];

      environment = lib.mapAttrs (_: toEnv) cfg.settings;

      serviceConfig = {
        ExecStart = lib.getExe cfg.package;
        Restart = "on-failure";
        RestartSec = "10s";

        User = name;
        Group = name;
        StateDirectory = name;
        # Inkheart does not create the cover and thumbnail dirs itself.
        CacheDirectory = [
          name
          "${name}/covers"
          "${name}/thumbnails"
        ];

        ProtectSystem = "strict";
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

    belak.services.caddy.virtualHosts.${cfg.domain}.backend = "http://127.0.0.1:${toString cfg.port}";

    belak.services.oauth2-proxy.instances = lib.mkIf cfg.oauth2Proxy.enable {
      ${name} = {
        inherit (cfg) domain;
        inherit (cfg.oauth2Proxy) port;
        envFile = config.age.secrets.inkheart-oauth2-proxy-env.path;
      };
    };

    age.secrets.inkheart-oauth2-proxy-env = lib.mkIf cfg.oauth2Proxy.enable {
      file = ../../../../secrets/inkheart-oauth2-proxy-env.age;
    };

    # The agenix path never changes, so restart on the secret's hash.
    systemd.services."oauth2-proxy-${name}" = lib.mkIf cfg.oauth2Proxy.enable {
      restartTriggers = [
        (builtins.hashFile "sha256" config.age.secrets.inkheart-oauth2-proxy-env.file)
      ];
    };
  };
}
