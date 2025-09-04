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
    ./disko-config.nix
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "freya";
    domain = "elwert.dev";
  };

  environment.systemPackages = with pkgs; [
    nfs-utils
  ];

  belak = {
    server.enable = true;

    services.plex.enable = true;
  };

  fileSystems = {
    "/mnt/garnet" = {
      device = "garnet.elwert.dev:/volume1/Media";
      fsType = "nfs";
    };

    "/mnt/plex/Movies" = {
      device = "/mnt/garnet/Movies";
      options = ["bind" "ro"];
    };

    "/mnt/plex/TV" = {
      device = "/mnt/garnet/TV";
      options = ["bind" "ro"];
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
