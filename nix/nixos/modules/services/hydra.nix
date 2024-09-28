{ config, lib, ... }:
let
  cfg = config.belak.services.hydra;
in
{
  options.belak.services.hydra = {
    enable = lib.mkEnableOption "hydra";

    domain = lib.mkOption { default = "hydra.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.hydra = {
      enable = true;
      hydraURL = "https://${cfg.domain}"; # externally visible URL
      notificationSender = "hydra@localhost";
      buildMachinesFiles = [ ];
      useSubstitutes = true;
      extraConfig = ''
        using_frontend_proxy 1
        base_uri ${cfg.domain}
      '';
    };

    belak.acme.enable = true;

    services.nginx.virtualHosts."${cfg.domain}" = {
      useACMEHost = "primary";
      forceSSL = true;

      locations."/".proxyPass = "http://localhost:3000";
    };
  };
}
