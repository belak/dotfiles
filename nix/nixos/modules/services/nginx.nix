{ config, lib, ... }:
let
  cfg = config.belak.services.nginx;
in
{
  options.belak.services.nginx = {
    enable = lib.mkEnableOption "nginx";
  };

  config = lib.mkIf cfg.enable {
    services.nginx = {
      enable = true;

      recommendedOptimisation = true;

      # Note that we actually cannot use recommendedProxySettings as it will
      # nuke headers set by our haproxy instance, causing X-Forwarded-* headers
      # to be lost.
      #recommendedProxySettings = true;
    };

    users.users.nginx = {
      group = "nginx";
      isSystemUser = true;
    };

    users.groups.nginx = { };

    networking.firewall.allowedTCPPorts = [
      80
    ];

    # Hacks - disable ProtectHome so we can access sockets outside our home.
    # There should be a better way to do this, but this works for now.
    systemd.services.nginx.serviceConfig.ProtectHome = false;
  };
}
