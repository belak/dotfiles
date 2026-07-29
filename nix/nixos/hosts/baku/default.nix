{ modulesPath, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "baku";
    domain = "elwert.dev";
  };

  belak = {
    server.enable = true;
    services = {
      forgejo.enable = true;
      nginx.enable = true;
    };
  };

  security.sudo.enable = true;
  services.openssh.enable = true;

  system.stateVersion = "26.05";
}
