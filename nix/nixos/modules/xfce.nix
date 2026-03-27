{
  config,
    lib,
    pkgs,
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
    services.xserver = {
      enable = true;
      desktopManager.xfce.enable = true;
    };

    services.picom.enable = true;
  };
}
