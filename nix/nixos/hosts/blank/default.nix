{ modulesPath, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "blank";
    domain = "elwert.dev";
  };

  belak = {
    server.enable = true;
  };

  security.sudo.enable = true;
  services.openssh.enable = true;

  system.stateVersion = "26.05";
}
