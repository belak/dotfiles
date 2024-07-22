{ config, lib, ... }:
let
  cfg = config.belak.services.postgres;
in
{
  options.belak.services.postgres = {
    enable = lib.mkEnableOption "postgres";
  };

  config = lib.mkIf cfg.enable {
    services.postgresql = {
      enable = true;
    };
  };
}
