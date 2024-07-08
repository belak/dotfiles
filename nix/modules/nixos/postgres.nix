{ config, lib, ... }:
let
  cfg = config.belak.postgres;
in
{
  options.belak.postgres = {
    enable = lib.mkEnableOption "postgres";
  };

  config = lib.mkIf cfg.enable {
    services.postgres = {
      enable = true;
    };
  };
}
