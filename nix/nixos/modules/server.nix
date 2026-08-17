{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.server;

  # The same keys that are agenix recipients in secrets/secrets.nix are used for
  # root ssh access.
  keys = import ../../../secrets/keys.nix;
in
{
  options.belak.server = {
    enable = lib.mkEnableOption "server";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      lm_sensors
      molly-guard
      nfs-utils
    ];

    services.openssh.enable = true;

    users.users.root.openssh.authorizedKeys.keys = keys.users;

    # We use mkForce because we want to override the default values.
    time.timeZone = lib.mkForce "Etc/UTC";

    # For laptops, this will make it so they can be run closed. This should have
    # no effect on other hardware.
    services.logind.settings.Login.HandleLidSwitch = "ignore";
  };
}
