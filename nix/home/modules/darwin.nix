{
  pkgs,
  lib,
  config,
  ...
}:
{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    home.packages = with pkgs; [
      coreutils-prefixed
      gnugrep
      mkalias
    ];

    # This makes applications show up under spotlight, without needing to manage
    # it manually. It's enabled by default with home-manager state version >=
    # 25.11, but many of my setups are older than that.
    targets.darwin.copyApps.enable = true;
    targets.darwin.linkApps.enable = false;
  };
}
