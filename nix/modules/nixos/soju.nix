{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.soju;
  certCfg = config.security.acme.certs.soju;

  stateDir = "/var/lib/soju";
  #runtimeDir = "/run/soju";

  configFile = pkgs.writeText "soju.conf" ''
    listen irc+insecure://:6667
    listen ircs://:7000
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
  options.belak.soju = {
    enable = lib.mkEnableOption "soju";
  };

  config = lib.mkIf cfg.enable {
    # NOTE: services.soju does exist, but it uses DynamicUser on the systemd
    # unit file which breaks ACME cert generation. There's no way to know what
    # will change in the future, so it's less error prone to just define the
    # whole thing ourselves.

    environment.systemPackages = [ sojuctl ];

    systemd.services.soju = {
      description = "Soju IRC bouncer";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [
        "network-online.target"
        "acme-selfsigned-soju.target"
      ];
      requires = [ "acme-finished-soju.target" ];
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

    security.acme.certs.soju = {
      domain = "soju.elwert.cloud";
      group = "soju";
      reloadServices = [ "soju" ];
    };

    networking.firewall.allowedTCPPorts = [
      6667
      7000
    ];
  };
}
