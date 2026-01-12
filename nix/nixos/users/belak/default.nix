{ pkgs, config, ... }:
let
  ifTheyExist = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
  extraGroups = ifTheyExist [ "networkmanager" ];
in
{
  users.users.belak = {
    home = "/home/belak";
    isNormalUser = true;
    hashedPasswordFile = config.age.secrets.belak-password.path;
    description = "Kaleb Elwert";
    extraGroups = [
      "cdrom"
      "wheel"
      "dialout"
    ]
    ++ extraGroups;
    shell = pkgs.zsh;

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

  age.secrets.belak-password.file = ../../../../secrets/belak-password.age;
}
