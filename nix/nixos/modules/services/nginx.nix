{ config, lib, ... }:
let
  cfg = config.belak.services.nginx;
in
{
  options.belak.services.nginx = {
    enable = lib.mkEnableOption "nginx";
    enableTls = lib.mkEnableOption "nginx-tls";
  };

  config = lib.mkIf cfg.enable {
    services.nginx = {
      enable = true;

      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;

      commonHttpConfig = ''
        add_header X-Clacks-Overhead "GNU Douglas Adams";
        add_header X-Clacks-Overhead "GNU Robert Asprin";
      '';
    };

    belak.acme.enable = cfg.enableTls;

    security.acme.certs.primary = lib.mkIf cfg.enableTls {
      domain = "${config.networking.hostName}.${config.networking.domain}";
      extraDomainNames = [
        "homelab.elwert.dev"
        "*.elwert.cloud"
      ];
      group = config.services.nginx.group;
    };

    security.acme.certs.seabird = lib.mkIf cfg.enableTls {
      domain = "seabird.chat";
      extraDomainNames = [
        "*.seabird.chat"
      ];
      group = config.services.nginx.group;
    };

    networking.firewall.allowedTCPPorts = [
      80
      443
    ];
  };
}
