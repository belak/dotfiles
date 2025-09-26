{ config, lib, ... }:
let
  cfg = config.belak.dev;
in
{
  options.belak.dev = {
    enable = lib.mkEnableOption "devtools";
    armEmulation = lib.mkEnableOption "arm emulation";
  };

  config = lib.mkIf cfg.enable {
    boot.binfmt.emulatedSystems = lib.mkIf cfg.armEmulation [ "aarch64-linux" ];
  };
}
