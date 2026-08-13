# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).
{
  lib,
  config,
  pkgs,
  microvm,
  ...
}:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    microvm.nixosModules.host
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "eiko";
    domain = "elwert.dev";

    # systemd-networkd makes it much easier to attach microvm interfaces to
    # a bridge.
    useNetworkd = true;

    # Note that we need to disable the global DHCP client to avoid requesting
    # leases for bridges which end up blackholing guest traffic.
    useDHCP = false;
    interfaces.eno1.useDHCP = true;

    bridges.br-seabird.interfaces = [ "eno1" ];
    interfaces.br-seabird.useDHCP = false;
  };

  # The bridge doesn't have an address, but we don't want it to hold up
  # network-online.target.
  systemd.network.networks."40-br-seabird".linkConfig.RequiredForOnline = "no";

  # Guest taps are created by the guest's own runner, so match on the name
  # prefix rather than naming each VM here.
  systemd.network.networks."11-microvm-seabird" = {
    matchConfig.Name = "vm-*";
    networkConfig.Bridge = "br-seabird";
    linkConfig.RequiredForOnline = "no";
  };

  microvm.autostart = [ ];

  belak = {
    server.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
