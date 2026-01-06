{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.services.soju;
  certCfg = config.security.acme.certs.soju;

  stateDir = "/var/lib/soju";
  #runtimeDir = "/run/soju";

  configFile = pkgs.writeText "soju.conf" ''
    listen ircs://:7000
    listen ws+insecure://localhost:7001
    hostname soju.elwert.cloud
    tls ${certCfg.directory}/fullchain.pem ${certCfg.directory}/key.pem
    db sqlite3 ${stateDir}/soju.db
    log fs ${stateDir}/logs
  '';

  sojuctl = pkgs.writeShellScriptBin "sojuctl" ''
    exec ${pkgs.soju}/bin/sojuctl --config ${configFile} "$@"
  '';
in
{
  options.belak.services.soju = {
    enable = lib.mkEnableOption "soju";
    domain = lib.mkOption { default = "soju.elwert.cloud"; };
    gamjaDomain = lib.mkOption { default = "irc.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    # NOTE: services.soju does exist, but it uses DynamicUser on the systemd
    # unit file which breaks ACME cert generation. There's no way to know what
    # will change in the future, so it's less error prone to just define the
    # whole thing ourselves.

    environment.systemPackages = [
      sojuctl
    ];

    systemd.services.soju = {
      description = "Soju IRC bouncer";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [
        "network-online.target"
        "acme-soju.service"
      ];
      requires = [ "acme-soju.service" ];
      serviceConfig = {
        Restart = "always";
        ExecStart = "${pkgs.soju}/bin/soju -config ${configFile}";
        StateDirectory = "soju";
        RuntimeDirectory = "soju";
        User = "soju";
        Group = "soju";
      };
    };

    users.users.soju = {
      group = "soju";
      isSystemUser = true;
    };

    users.groups.soju = { };

    belak.acme.enable = true;

    services.nginx.virtualHosts."${cfg.gamjaDomain}" = {
      locations."/".root = pkgs.gamja;

      locations."/socket" = {
        proxyPass = "http://localhost:7001";
        proxyWebsockets = true;
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_read_timeout 600s;
        '';
      };
    };

    security.acme.certs.soju = {
      inherit (cfg) domain;
      group = "soju";
      reloadServices = [ "soju" ];
    };

    networking.firewall.allowedTCPPorts = [ 7000 ];
  };
}
