{ modulesPath, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "marcus";
    domain = "elwert.dev";
  };

  belak = {
    server.enable = true;

    services.woodpecker-agent = {
      enable = true;
      maxWorkflows = 4;
    };
  };

  security.sudo.enable = true;
  services.openssh.enable = true;

  system.stateVersion = "26.05";
}
