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

  # Stock power limits (25W/28s long, 30W short) let sustained all-core load
  # run hotter than this chassis can dissipate, which caused kernel panics at
  # 100C. PL1 is what keeps it out of that range; the thermal target is an
  # independent backstop.
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

  system.stateVersion = "26.05";
}
