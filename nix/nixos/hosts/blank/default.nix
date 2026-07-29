{ modulesPath, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "blank";
    domain = "elwert.dev";
  };

  security.sudo.enable = true;
  services.openssh.enable = true;

  system.stateVersion = "26.05";
}
