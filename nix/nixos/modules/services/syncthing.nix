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
    #
    # The trigger hashes the file rather than pointing at it: inside a flake a
    # path is a subpath of the whole source tree, so its store hash moves on any
    # repo change, not just this secret.
    systemd.services.syncthing.restartTriggers = [
      (builtins.hashFile "sha256" config.age.secrets.syncthing-gui-password.file)
    ];

    # Without this, syncthing can start before dataDir is mounted and create
    # folder markers in the empty mountpoint, which then shadow the real
    # folders once the mount appears.
    systemd.services.syncthing.unitConfig.RequiresMountsFor = syncthingCfg.dataDir;
  };
}
