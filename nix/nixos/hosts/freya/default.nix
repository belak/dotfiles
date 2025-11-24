# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).
{
  lib,
  config,
  pkgs,
  ...
}:
let
  mc-calzone-rcon =
  (pkgs.writeShellScriptBin "mc-calzone-rcon" ''
    ${pkgs.mcrcon}/bin/mcrcon -P 25575 -p $(cat /var/lib/minecraft/all-the-calzones/server.properties | grep rcon.password | cut -b 15-) $@
  '');
in
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
    mc-calzone-rcon
  ];

  belak = {
    server.enable = true;

    services.jellyfin.enable = true;
    services.plex.enable = true;
  };

  fileSystems = {
    "/mnt/garnet" = {
      device = "garnet.elwert.dev:/volume1/Media";
      fsType = "nfs";
    };

    "/mnt/media/Movies" = {
      device = "/mnt/garnet/Movies";
      options = [
        "bind"
        "ro"
      ];
    };

    "/mnt/media/TV" = {
      device = "/mnt/garnet/TV";
      options = [
        "bind"
        "ro"
      ];
    };

    "/mnt/plex/Movies" = {
      device = "/mnt/garnet/Movies";
      options = [
        "bind"
        "ro"
      ];
    };

    "/mnt/plex/TV" = {
      device = "/mnt/garnet/TV";
      options = [
        "bind"
        "ro"
      ];
    };
  };

  users.users.minecraft-all-the-calzones = {
    group = "minecraft-all-the-calzones";
    isSystemUser = true;
  };

  users.groups.minecraft-all-the-calzones = { };

  systemd.services.minecraft-all-the-calzones = {
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    path = with pkgs; [
      openjdk21_headless
    ];
    serviceConfig = {
      Restart = "always";
      ExecStart = "${pkgs.bash}/bin/bash /var/lib/minecraft/all-the-calzones/run.sh";
      WorkingDirectory = "/var/lib/minecraft/all-the-calzones";
      StateDirectory = "minecraft/all-the-calzones";
      ExecStop = "${mc-calzone-rcon}/bin/mc-calzone-rcon stop";
      User = "minecraft-all-the-calzones";
      Group = "minecraft-all-the-calzones";
    };
  };

  networking.firewall.allowedTCPPorts = [
    8080
    25565
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
