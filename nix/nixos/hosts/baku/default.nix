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
    };
  };

  security.sudo.enable = true;
  services.openssh.enable = true;

  system.stateVersion = "26.05";
}
