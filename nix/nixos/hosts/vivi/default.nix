{ ... }:
{
  imports = [
    ./disko-config.nix
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "vivi";
    domain = "elwert.dev";
  };

  belak = {
    server.enable = true;

    services = {
      atticd.enable = true;
      forgejo.enable = true;
      nginx.enable = true;

      woodpecker = {
        enable = true;
        open = true;
        admins = [
          "forgejo-admin"
          "belak"
        ];
        repoOwners = [
          "belak"
          "seabird-chat"
        ];
      };

      # Server and agent share the host, but the agent still goes over gRPC so
      # that adding a remote agent later needs no server-side changes.
      #
      # Two workflows rather than four: this is a 15W mobile part, so more
      # parallel nix builds mostly buys throttling, and four was enough to
      # lock the box up under a mix of rust and go builds.
      woodpecker-agent = {
        enable = true;
        maxWorkflows = 2;
      };
    };
  };

  security.sudo.enable = true;
  services.openssh.enable = true;

  # This box has hung hard every so often for 5+ years, at idle and under
  # load, leaving nothing on disk - it stops logging mid-line and sits locked
  # until it is power cycled. Replacing the bad RAM helped but did not fix it,
  # so the cause is still unknown; the goal here is to recover without a power
  # cycle and to capture something next time.
  #
  # A hard lockup only warns by default, and the warning never reaches disk;
  # panicking instead gets it into pstore. The hardware watchdog reboots the
  # machine even when the kernel is too wedged to panic, which also avoids the
  # unclean shutdowns that have corrupted SQLite databases here.
  #
  # Firmware does not support SETTIMEOUT, so the 60s timeout is fixed.
  boot.kernel.sysctl."kernel.hardlockup_panic" = 1;
  boot.kernelParams = [ "panic=10" ];

  systemd.settings.Manager = {
    RuntimeWatchdogSec = "60s";
    RebootWatchdogSec = "3m";
  };

  system.stateVersion = "26.05";
}
