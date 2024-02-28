{ config, lib, pkgs, ... }:
let
  cfg = config.belak.server;
in
{
  options.belak.server = {
    enable = lib.mkEnableOption "server";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ molly-guard ];
  };
}
