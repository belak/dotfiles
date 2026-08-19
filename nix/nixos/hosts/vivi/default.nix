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
      # Two workflows rather than four: this is a 15W mobile part, so more
      # parallel nix builds mostly buys throttling.
      woodpecker-agent = {
        enable = true;
        maxWorkflows = 2;
      };
    };
  };

  security.sudo.enable = true;
  services.openssh.enable = true;

  # Years of hard lockups on this host turned out to be bad RAM.
  #
  # Keep memtest a boot menu entry away rather than needing a USB stick.
  boot.loader.systemd-boot.memtest86.enable = true;

  system.stateVersion = "26.05";
}
