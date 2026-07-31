{ config, lib, ... }:
let
  cfg = config.belak.services.syncthing;
  syncthingCfg = config.services.syncthing;
in
{
  options.belak.services.syncthing = {
    enable = lib.mkEnableOption "syncthing";
    domain = lib.mkOption { default = "syncthing.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      guiPasswordFile = config.age.secrets.syncthing-gui-password.path;
      openDefaultPorts = true;
      dataDir = "/mnt/syncthing";
      configDir = "/var/lib/syncthing";
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."/".proxyPass = "http://127.0.0.1:8384";
    };

    age.secrets.syncthing-gui-password = {
      file = ../../../../secrets/syncthing-gui-password.age;
      group = syncthingCfg.group;
    };

    # guiPasswordFile points at a stable /run/agenix path, so
    # switch-to-configuration won't restart the service on its own when
    # the secret's content changes.
    systemd.services.syncthing.restartTriggers = [ config.age.secrets.syncthing-gui-password.file ];
  };
}
