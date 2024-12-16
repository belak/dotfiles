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
  };

  config = lib.mkIf cfg.enable {
    console = with pkgs; {
      earlySetup = true;
      font = "${terminus_font}/share/consolefonts/ter-114n.psf.gz";
      packages = [ terminus_font ];
    };

    environment.systemPackages = with pkgs; [
      acpi
      gparted
      powertop
    ];
  };
}
