{ config, lib, ... }:
let
  cfg = config.belak.services.miniflux;
in
{
  options.belak.services.miniflux = {
    enable = lib.mkEnableOption "miniflux";
    domain = lib.mkOption { default = "rss.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.miniflux = {
      enable = true;
      config = {
        LISTEN_ADDR = "/run/miniflux/miniflux.sock";
      };
      adminCredentialsFile = config.age.secrets.miniflux-admin-credentials.path;
    };

    belak.acme.enable = true;

    services.nginx.virtualHosts."${cfg.domain}" = {
      useACMEHost = "primary";
      forceSSL = true;
      locations."/".proxyPass = "http://unix:/run/miniflux/miniflux.sock";
    };

    age.secrets.miniflux-admin-credentials.file = ../../../../secrets/miniflux-admin-credentials.age;
  };
}
