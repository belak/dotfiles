{ config, lib, ... }:
let
  cfg = config.belak.services.soju;
in
{
  options.belak.services.soju = {
    enable = lib.mkEnableOption "soju";
  };

  config = lib.mkIf cfg.enable {
    services.soju = {
      enable = true;
      hostName = "soju.elwert.cloud";
      listen = [ "irc+insecure://:6667" ];
    };

    networking.firewall.allowedTCPPorts = [ 6667 ];
  };
}
