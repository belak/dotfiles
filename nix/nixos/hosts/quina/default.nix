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
    dev.enable = true;
    laptop = {
      enable = true;
      consoleFont = "ter-124n";
    };
  };

  home-manager.users.belak = ../../../home/users/belak/quina.nix;

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      nerd-fonts.symbols-only
    ];
  };

  networking.networkmanager.enable = true;

  services.fwupd.enable = true;
  services.openssh.enable = true;
  services.pulseaudio.enable = true;

  # Because we use a GUI on this computer but aren't using a full DE, we need to
  # enable a few things that the Gnome module would otherwise do for us.
  hardware.graphics.enable = true;
  programs.dconf.enable = true;

  environment.systemPackages = with pkgs; [
    solaar
    swayidle
  ];

  programs.gtklock = {
    enable = true;
    modules = with pkgs; [
      gtklock-powerbar-module
    ];
  };

  #xdg.portal = {
  #  enable = true;
  #  extraPortals = [ pkgs.xdg-desktop-portal-gnome ];
  #  configPackages = [ pkgs.niri ];
  #};

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.04"; # Did you read the comment?
}
