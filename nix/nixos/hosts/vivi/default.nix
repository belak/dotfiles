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

      # Server and agent share the host; the agent still goes over gRPC so the
      # setup matches quina's remote agent.
      #
      # Two workflows rather than four: the undervolt holds this box to a 15W
      # PL1, so more parallel nix builds mostly buys throttling.
      woodpecker-agent = {
        enable = true;
        maxWorkflows = 2;
      };
    };
  };

  security.sudo.enable = true;
  services.openssh.enable = true;

  # Caps the package at 15W. Keep this: before it was added the box locked up
  # regularly under high load, and afterwards the hangs became infrequent. The
  # mechanism is unknown - with the cap in place the package settles around
  # 71C against a 100C crit point, and the hangs leave no thermal or MCE
  # events behind, so it is not simply overheating.
  services.undervolt = {
    enable = true;

    # An absolute Celsius target, not an offset from 100.
    temp = 85;

    p1 = {
      limit = 15;
      window = 28;
    };

    p2 = {
      limit = 25;
      window = 0.00244140625;
    };
  };

  # This box has hung hard every so often for 5+ years, at idle and under
  # load, leaving nothing on disk - it stops logging mid-line and sits locked
  # until it is power cycled. The cause is still unknown, so the goal here is
  # to recover without a power cycle and to capture something next time.
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

  # Marginal RAM is one candidate for the hangs above, so keep memtest a boot
  # menu entry away rather than needing a USB stick.
  boot.loader.systemd-boot.memtest86.enable = true;

  system.stateVersion = "26.05";
}
