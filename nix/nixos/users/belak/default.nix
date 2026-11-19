{ pkgs, config, ... }:
let
  ifTheyExist = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
  extraGroups = ifTheyExist [ "networkmanager" ];
in
{
  users.users.belak = {
    home = "/home/belak";
    isNormalUser = true;
    # Provisioned by hand on each machine; see README.md in this directory.
    hashedPasswordFile = "/etc/secrets/belak-password";
    description = "Kaleb Elwert";
    extraGroups = [
      "cdrom"
      "wheel"
      "dialout"
    ]
    ++ extraGroups;
    shell = pkgs.fish;

    # subuid and subgid allows podman to work as expected.
    subUidRanges = [
      {
        startUid = 100000;
        count = 65536;
      }
    ];
    subGidRanges = [
      {
        startGid = 100000;
        count = 65536;
      }
    ];
  };
}
