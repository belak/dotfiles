{
  config,
  lib,
  ...
}:
let
  cfg = config.belak.cosmic;
in
{
  options.belak.cosmic = {
    enable = lib.mkEnableOption "cosmic";
  };

  config = lib.mkIf cfg.enable {
    services.desktopManager.cosmic.enable = true;
    services.displayManager.cosmic-greeter.enable = true;
  };
}
