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

  belak = {
    laptop.enable = true;
    server.enable = true;

    acme.enable = true;

    services = {
      haproxy = {
        enable = true;
        acmeCerts = [
          "primary"
          "homelab"
          "seabird"
        ];
        backends = {
          eiko = {
            servers.eiko = "eiko.elwert.dev:80";

            matchers = [
              "if { req.hdr(host) -i eiko.elwert.dev }"
              "if { req.hdr(host) -i git.elwert.cloud }"
              "if { req.hdr(host) -i lldap.elwert.cloud }"
              "if { req.hdr(host) -i auth.elwert.cloud }"
              "if { req.hdr(host) -i rss.elwert.cloud }"
              "if { req.hdr(host) -i soju.elwert.cloud }"
              "if { req.hdr(host) -i gamja.elwert.cloud }"
            ];
          };

          steiner = {
            servers.steiner = "steiner.elwert.dev:80";

            matchers = [
              "if { req.hdr(host) -i steiner.elwert.dev }"
              "if { req.hdr(host) -i old-git.elwert.cloud }"
              "if { req.hdr(host) -i cloud.elwert.cloud }"
              "if { req.hdr(host) -i files.elwert.cloud }"
              "if { req.hdr(host) -i jellyfin.elwert.cloud }"
              "if { req.hdr(host) -i btta-api.elwert.cloud }"
              "if { req.hdr(host) -i btta-media.elwert.cloud }"
            ];
          };
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
    group = config.services.haproxy.group;
    reloadServices = [ "haproxy" ];
  };

  security.acme.certs.homelab = {
    domain = "*.elwert.dev";
    group = config.services.haproxy.group;
    reloadServices = [ "haproxy" ];
  };

  security.acme.certs.seabird = {
    domain = "seabird.chat";
    extraDomainNames = [
      "*.seabird.chat"
    ];
    group = config.services.haproxy.group;
    reloadServices = [ "haproxy" ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
