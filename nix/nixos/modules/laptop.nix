{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.laptop;
in
{
  options.belak.laptop = {
    enable = lib.mkEnableOption "laptop";
    consoleFont = lib.mkOption {
      type = lib.types.str;
      default = "ter-114n";
    };
  };

  config = lib.mkIf cfg.enable {
    console = with pkgs; {
      #earlySetup = true;
      font = "${terminus_font}/share/consolefonts/${cfg.consoleFont}.psf.gz";
      packages = [ terminus_font ];
    };

    environment.systemPackages = with pkgs; [
      acpi
      gparted
      powertop
    ];
  };
}
