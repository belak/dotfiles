{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.server;
in
{
  options.belak.server = {
    enable = lib.mkEnableOption "server";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ molly-guard ];

    services.openssh.enable = true;

    # We use mkOverride because we want to override the default values.
    time.timeZone = lib.mkOverride "Etc/UTC";

    # For laptops, this will make it so they can be run closed. This should have
    # no effect on other hardware.
    services.logind.lidSwitch = "ignore";
  };
}
