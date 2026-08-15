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
