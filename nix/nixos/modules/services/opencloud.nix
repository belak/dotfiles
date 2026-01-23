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
      enable = false;
      url = "https://${cfg.domain}";
      environment = {
        PROXY_TLS = "true";
      };
      environmentFile = config.age.secrets.opencloud-env.path;
    };

    age.secrets.opencloud-env = {
      file = ../../../../secrets/opencloud-env.age;
      #owner = opencloudConfig.user;
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."/".proxyPass = "http://localhost:${toString opencloudConfig.port}";
    };
  };
}
