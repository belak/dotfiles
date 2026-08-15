# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).
{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./disko-config.nix
  ];

  networking = {
    hostName = "quina";
    domain = "elwert.dev";
  };

  belak = {
    laptop = {
      enable = true;
      consoleFont = "ter-124n";
    };
    server.enable = true;

    # Kept on solely as the arm64 woodpecker agent, so that decommissioning it
    # later costs nothing but CI capacity. atticd moved to vivi, and nginx was
    # only ever here to front it.
    services.woodpecker-agent = {
      enable = true;
      maxWorkflows = 4;
    };
  };

  services.openssh.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?
}
