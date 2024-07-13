{ config, lib, ... }:
let
  cfg = config.belak.nginx;
in
{
  options.belak.nginx = {
    enable = lib.mkEnableOption "nginx";
  };

  config = lib.mkIf cfg.enable {
    services.nginx = {
      enable = true;

      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
    };

    security.acme.certs.primary = {
      domain = "${config.networking.hostName}.${config.networking.domain}";
      extraDomainNames = [
        "homelab.elwert.dev"
        "*.elwert.cloud"
      ];
      group = config.services.nginx.group;
    };

    networking.firewall.allowedTCPPorts = [
      80
      443
    ];
  };
}
