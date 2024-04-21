{ pkgs, config, ... }:
let
  ifTheyExist = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
  extraGroups = ifTheyExist [ "networkmanager" ];
in
{
  users.users.belak = {
    home = "/home/belak";
    isNormalUser = true;
    initialPassword = "hunter2";
    description = "Kaleb Elwert";
    extraGroups = [
      "wheel"
      "dialout"
    ] ++ extraGroups;
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
}
