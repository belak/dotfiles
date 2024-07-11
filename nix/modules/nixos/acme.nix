{ config, lib, ... }:
let
  cfg = config.belak.acme;
in
{
  options.belak.acme = {
    enable = lib.mkEnableOption "acme";
  };

  config = lib.mkIf cfg.enable {
    security.acme = {
      acceptTerms = true;

      defaults = {
        email = "kaleb+acme@coded.io";
        dnsProvider = "cloudflare";
        environmentFile = config.age.secrets.acme-cloudflare-env.path;
      };
    };
  };
}
