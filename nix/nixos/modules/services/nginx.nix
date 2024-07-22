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
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
    };

    belak.acme.enable = true;

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
