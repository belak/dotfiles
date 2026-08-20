{ ... }:
{
  imports = [
    ./disko-config.nix
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "garnet";
    domain = "elwert.dev";
  };

  belak = {
    server.enable = true;
  };

  system.stateVersion = "26.05";
}
