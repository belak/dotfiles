{ config, lib, pkgs, ... }:
let
  cfg = config.belak.services.btta;
  socket = "/run/btta/btta.sock";
  stateDir = "/var/lib/btta";

  bttactl = pkgs.writeShellScriptBin "bttactl" ''
    export BTTA_DB="${stateDir}/btta.db"
    export BTTA_MEDIA_DIR="${stateDir}/media"
    exec ${pkgs.belak-btta}/bin/btta "$@"
  '';
in
{
  options.belak.services.btta = {
    enable = lib.mkEnableOption "btta";
    domain = lib.mkOption { default = "btta.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    users.users.btta = {
      group = "btta";
      isSystemUser = true;
    };

    users.groups.btta = {
      members = [ "nginx" ];
    };

    environment.systemPackages = [ bttactl pkgs.sqlite ];

    systemd.services.btta = {
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        Restart = "always";
        ExecStart = "${pkgs.belak-btta}/bin/btta serve -bind unix:${socket}";
        Environment = [
          "BTTA_DB=${stateDir}/btta.db"
          "BTTA_MEDIA_DIR=${stateDir}/media"
        ];
        RuntimeDirectory = "btta";
        StateDirectory = "btta";
        User = "btta";
        Group = "btta";
      };
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."/".proxyPass = "http://unix:${socket}";
    };
  };
}
