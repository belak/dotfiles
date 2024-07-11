{ config, lib, ... }:
let
  cfg = config.belak.soju;
  certCfg = config.security.acme.certs.soju;
in
{
  options.belak.soju = {
    enable = lib.mkEnableOption "soju";
  };

  config = lib.mkIf cfg.enable {
    services.soju = {
      enable = true;
      hostName = "soju.elwert.cloud";
      listen = [ "ircs://:7000" ];
      tlsCertificate = "${certCfg.directory}/soju/fullchain.pem";
      tlsCertificateKey = "${certCfg.directory}/soju/key.pem";
    };

    #systemd.services.soju = {
    #  after = [ "acme-selfsigned-soju.target" ];
    #  requires = [ "acme-finished-soju.target" ];
    #}

    security.acme.certs.soju = {
      domain = "soju.elwert.cloud";
      group = "soju";
      reloadServices = [ "soju" ];
    };

    networking.firewall.allowedTCPPorts = [ 7000 ];
  };
}
