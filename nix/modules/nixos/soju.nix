{ config, lib, ... }:
let
  cfg = config.belak.soju;
in
{
  options.belak.soju = {
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
