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

  networking = {
    hostName = "zidane";
    domain = "elwert.dev";
  };

  environment.systemPackages = with pkgs; [
    sqlite
  ];

  belak = {
    laptop.enable = true;
    server.enable = true;

    acme.enable = true;

    services.nginx = {
      enable = true;
      # Caddy already owns port 80/443 on this host.
      port = 8080;
    };

    services.pocket-id.enable = true;

    services.soju.enable = true;

    services.caddy = {
      enable = true;

      virtualHosts = {
        # Hosted here (zidane)
        "belak.io" = {
          extraHosts = [
            "www.belak.io"
          ];
          backend = "http://localhost:8081";
          useACMEHost = "blog";
        };

        "beta.belak.io" = {
          backend = "http://localhost:8081";
          useACMEHost = "blog";
        };

        "pocket-id.elwert.cloud" = {
          backend = "http://localhost:8080";
        };
        "irc.elwert.cloud" = {
          backend = "http://localhost:8080";
        };

        # Hosted on vivi
        "forgejo.elwert.cloud" = {
          backend = "http://vivi.elwert.dev";
        };
        "woodpecker.elwert.cloud" = {
          backend = "http://vivi.elwert.dev";
        };
        "attic.elwert.cloud" = {
          backend = "http://vivi.elwert.dev";
        };

        # Hosted on freya
        "audiobookshelf.elwert.cloud" = {
          extraHosts = [
            "audiobooks.elwert.cloud"
          ];
          backend = "http://freya.elwert.dev";
        };
        "btta.elwert.cloud" = {
          backend = "http://freya.elwert.dev";
        };
        "immich.elwert.cloud" = {
          backend = "http://freya.elwert.dev";
        };
        "photos.elwert.cloud" = {
          backend = "http://freya.elwert.dev";
        };
        "miniflux.elwert.cloud" = {
          backend = "http://freya.elwert.dev";
        };
        "kavita.elwert.cloud" = {
          backend = "http://freya.elwert.dev";
        };
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

        # Hosted on kupo (seabird)
        "seabird.chat" = {
          extraHosts = [
            "*.seabird.chat"
          ];
          backend = "http://kupo.infra.seabird.chat";
          useACMEHost = "seabird";
        };
        "staging.seabird.chat" = {
          extraHosts = [
            "*.staging.seabird.chat"
          ];
          backend = "http://stiltzkin.infra.seabird.chat";
          useACMEHost = "seabird";
        };
        "seabird-core.elwert.cloud" = {
          backend = "http://kupo.infra.seabird.chat";
        };
      };
    };
  };

  systemd.services.belak-blog = {
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      Restart = "always";
      ExecStart = "${pkgs.belak-blog}/bin/belak-blog -addr :8081";
    };
  };

  security.acme.certs.primary = {
    domain = "elwert.cloud";
    extraDomainNames = [
      "*.elwert.cloud"
    ];
  };

  security.acme.certs.blog = {
    domain = "belak.io";
    extraDomainNames = [
      "*.belak.io"
    ];
  };

  security.acme.certs.homelab = {
    domain = "*.elwert.dev";
  };

  security.acme.certs.seabird = {
    domain = "seabird.chat";
    extraDomainNames = [
      "*.seabird.chat"
      "*.staging.seabird.chat"
    ];
  };

  # Temporary during migration off synology
  fileSystems = {
    "/mnt/unas/media" = {
      device = "amarant.elwert.dev:/var/nfs/shared/Media";
      fsType = "nfs";
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
