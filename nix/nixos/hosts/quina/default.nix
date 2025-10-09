# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).
{ config, pkgs, ... }:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.installDeviceTree = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.deviceTree = {
    enable = true;
    name = "qcom/sc8280xp-lenovo-thinkpad-x13s.dtb";
  };

  boot.kernelParams = [
    "clk_ignore_unused"
    "pd_ignore_unused"
    "arm64.nopauth"
  ];

  boot.initrd.availableKernelModules = [
    "i2c-core"
    "i2c-hid"
    "i2c-hid-of"
    "i2c-qcom-geni"
    #"pcie-qcom"
    "phy-qcom-qmp-combo"
    "phy-qcom-qmp-pcie"
    "phy-qcom-qmp-usb"
    "phy-qcom-snps-femto-v2"
    "phy-qcom-usb-hs"
  ];

  networking = {
    hostName = "quina";
    domain = "elwert.dev";
  };

  belak = {
    dev.enable = true;
    #gnome.enable = true;
    laptop = {
      enable = true;
      consoleFont = "ter-124n";
    };
  };

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      nerd-fonts.symbols-only
    ];
  };

  networking.networkmanager.enable = true;

  services.openssh.enable = true;

  environment.systemPackages = with pkgs; [
    #niri
    #uwsm
    #xwayland-satellite
  ];

  #programs.uwsm.enable = true;

  # Configure UWSM to launch Hyprland from a display manager like SDDM
  #programs.uwsm.waylandCompositors = {
  #  niri = {
  #    prettyName = "Niri";
  #    comment = "Niri compositor managed by UWSM";
  #    binPath = "/run/current-system/sw/bin/niri";
  #  };
  #};

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
