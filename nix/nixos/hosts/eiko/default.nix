# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).
{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  belak = {
    server.enable = true;

    services = {
      forgejo.enable = true;
      miniflux.enable = true;
      nginx.enable = true;
      opencloud.enable = true;
      pocket-id.enable = true;
      postgres.enable = true;
      soju.enable = true;
    };
  };

  networking = {
    hostName = "eiko";
    domain = "elwert.dev";
  };

  # Temporary during migration off synology
  fileSystems = {
    # Base network filesystems
    "/mnt/amarant/media" = {
      device = "amarant.elwert.dev:/var/nfs/shared/Media";
      fsType = "nfs";
    };

    "/mnt/media" = {
      device = "/mnt/amarant/media";
      options = [
        "bind"
        "ro"
      ];
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
