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
      dmidecode
      ethtool
      fio
      iotop
      lm_sensors
      lsof
      memtester
      molly-guard
      nfs-utils
      nvme-cli
      pciutils
      smartmontools
      strace
      stress-ng
      sysstat
      tcpdump
      usbutils
    ];

    services.openssh.enable = true;

    # Disk health is only useful if something is actually watching it, so run
    # smartd rather than relying on manual smartctl runs.
    services.smartd.enable = true;

    # Keep memtest a boot menu entry away rather than needing a USB stick.
    # memtest86+ only builds for x86, so aarch64 hosts have to go without.
    boot.loader.systemd-boot.memtest86.enable = pkgs.stdenv.hostPlatform.isx86;

    users.users.root.openssh.authorizedKeys.keys = keys.users;

    # We use mkForce because we want to override the default values.
    time.timeZone = lib.mkForce "Etc/UTC";

    # For laptops, this will make it so they can be run closed. This should have
    # no effect on other hardware.
    services.logind.settings.Login.HandleLidSwitch = "ignore";
  };
}
