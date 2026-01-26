{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.belak.services.opencloud;
  opencloudConfig = config.services.opencloud;
in
{
  options.belak.services.opencloud = {
    enable = lib.mkEnableOption "opencloud";
    domain = lib.mkOption { default = "files.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.opencloud = {
      enable = true;
      url = "https://${cfg.domain}";
      environment = {
        PROXY_TLS = "false";
        OC_DOMAIN = cfg.domain;
        #OC_INSECURE = "true";
      };
      environmentFile = config.age.secrets.opencloud-env.path;
    };

    age.secrets.opencloud-env = {
      file = ../../../../secrets/opencloud-env.age;
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."/" = {
        proxyPass = "https://localhost:${toString opencloudConfig.port}";
        extraConfig = ''
          proxy_ssl_verify off;
        '';
        recommendedProxySettings = true;
      };
    };
  };
}
