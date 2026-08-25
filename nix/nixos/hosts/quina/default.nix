# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).
{ pkgs, ... }:
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

    gnome.enable = true;
  };

  services.fprintd.enable = true;

  services.pcscd.enable = true;
  services.pcscd.plugins = with pkgs; [ ccid ];

  services.openssh.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?
}
