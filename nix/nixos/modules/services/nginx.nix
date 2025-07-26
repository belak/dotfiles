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

      recommendedProxySettings = true;
      recommendedOptimisation = true;
    };

    users.users.nginx = {
      group = "nginx";
      isSystemUser = true;
    };

    users.groups.nginx = { };

    # Hacks - disable ProtectHome so we can access sockets outside our home.
    # There should be a better way to do this, but this works for now.
    systemd.services.nginx.serviceConfig.ProtectHome = false;
  };
}
