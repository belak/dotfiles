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
  boot.loader.efi.canTouchEfiVariables = true;

  environment.systemPackages = with pkgs; [
    sqlite
  ];

  belak = {
    laptop.enable = true;
    server.enable = true;

    acme.enable = true;

    services.caddy = {
      enable = true;

      virtualHosts = {
        # Hosted on eiko
        "gamja.elwert.cloud" = {
          backend = "http://eiko.elwert.dev";
        };
        "git.elwert.cloud" = {
          backend = "http://eiko.elwert.dev";
        };
        "lldap.elwert.cloud" = {
          backend = "http://eiko.elwert.dev";
        };
        "pocket-id.elwert.cloud" = {
          backend = "http://eiko.elwert.dev";
        };
        "rss.elwert.cloud" = {
          backend = "http://eiko.elwert.dev";
        };
        "soju.elwert.cloud" = {
          backend = "http://eiko.elwert.dev";
        };

        # Hosted on freya
        "jellyfin.elwert.cloud" = {
          backend = "http://freya.elwert.dev:8096";
        };
        "mc-calzone.elwert.cloud" = {
          backend = "http://freya.elwert.dev:8080";
        };
        "plex.elwert.cloud" = {
          backend = ''
            https://freya.elwert.dev:32400 {
              transport http {
                tls
                tls_server_name 192-168-30-6.63807cfbce034c3987141f96a950107d.plex.direct
              }
            }
          '';
        };

        # Hosted on garnet
        "garnet.elwert.dev" = {
          backend = "http://garnet.elwert.dev:5000";
          useACMEHost = "homelab";
        };

        # Hosted on steiner
        "elwert.cloud" = {
          extraHosts = [
            "www.elwert.cloud"
            "old-git.elwert.cloud"
            "cloud.elwert.cloud"
            "files.elwert.cloud"
            "emby.elwert.cloud"
            "btta-api.elwert.cloud"
            "btta-media.elwert.cloud"
          ];
          backend = "http://steiner.elwert.dev";
        };

        # Hosted on vivi (seabird)
        "api.seabird.chat" = {
          backend = "http://vivi.elwert.dev";
          useACMEHost = "seabird";
        };
        "webhooks.seabird.chat" = {
          backend = "http://vivi.elwert.dev";
          useACMEHost = "seabird";
        };
        "seabird-core.elwert.cloud" = {
          backend = "http://vivi.elwert.dev";
        };
      };
    };
  };

  networking = {
    hostName = "zidane";
    domain = "elwert.dev";
  };

  security.acme.certs.primary = {
    domain = "elwert.cloud";
    extraDomainNames = [
      "*.elwert.cloud"
    ];
  };

  security.acme.certs.homelab = {
    domain = "*.elwert.dev";
  };

  security.acme.certs.seabird = {
    domain = "seabird.chat";
    extraDomainNames = [
      "*.seabird.chat"
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
