{ modulesPath, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "marcus";
    domain = "elwert.dev";
  };

  security.sudo.enable = true;
  services.openssh.enable = true;

  system.stateVersion = "26.05";
}
