{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.belak.xfce;
in
{
  options.belak.xfce = {
    enable = lib.mkEnableOption "xfce";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
    ];

    xfconf = {
      enable = true;
    };
  };
}
