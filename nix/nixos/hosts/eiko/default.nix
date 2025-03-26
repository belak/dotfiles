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
      authelia.enable = true;
      gitea.enable = true;
      lldap.enable = true;
      miniflux.enable = true;
      nginx = {
        enable = true;
        enableTls = true;
      };
      postgres.enable = true;
      soju.enable = true;
    };
  };

  #services.seafile = {
  #  enable = true;
  #
  #  ccnetSettings.General.SERVICE_URL = "https://seafile.elwert.cloud";
  #  initialAdminPassword = "hunter2";
  #
  #  # 8083
  #};

  # We have a number of services which run on a host which hasn't been migrated
  # to NixOS, so we just forward them for now.
  #
  # Additionally, we use this host for TLS termination inside our network to
  # simplify the setup of other hosts.
  services.nginx.virtualHosts =
    (lib.genAttrs
      [
        "git.elwert.cloud"
        "cloud.elwert.cloud"
        "files.elwert.cloud"
        "jellyfin.elwert.cloud"
        "btta-api.elwert.cloud"
        "btta-media.elwert.cloud"
      ]
      (host: {
        useACMEHost = "primary";
        forceSSL = true;

        # steiner.elwert.dev
        locations."/".proxyPass = "https://192.168.30.3";
      })
    )
    // (lib.genAttrs
      [
        "ci.seabird.chat"
      ]
      (host: {
        useACMEHost = "seabird";
        forceSSL = true;

        # artemicion.elwert.dev
        locations."/".proxyPass = "http://192.168.30.14";

        locations."/ws" = {
          proxyPass = "http://192.168.30.14";
          proxyWebsockets = true;
        };
      })
    );

  networking = {
    hostName = "eiko";
    domain = "elwert.dev";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
