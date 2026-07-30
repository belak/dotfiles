{ config, lib, ... }:
let
  cfg = config.belak.services.podman;
in
{
  options.belak.services.podman = {
    enable = lib.mkEnableOption "podman";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.podman = {
      enable = true;

      # Required for containers to resolve each other by name.
      defaultNetwork.settings.dns_enabled = true;
    };

    # Needed for podman's DNS to work, per the upstream NixOS podman docs.
    networking.firewall.interfaces."podman0" = {
      allowedTCPPorts = [ 53 ];
      allowedUDPPorts = [ 53 ];
    };
  };
}
