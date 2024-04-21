{ pkgs, ... }:
{
  users.users.belak = {
    isNormalUser = true;
    initialPassword = "hunter2";
    description = "Kaleb Elwert";
    extraGroups = [
      "networkmanager"
      "wheel"
      "dialout"
    ];
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
