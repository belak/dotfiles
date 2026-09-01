{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.belak.apps;

  # Discord has no aarch64-linux build, which matters for quina. Test meta
  # rather than hardcoding the architecture so this starts working on its own
  # if that ever changes.
  discordAvailable = lib.meta.availableOn pkgs.stdenv.hostPlatform pkgs.discord;
in
{
  options.belak.apps = {
    enable = lib.mkEnableOption "apps";
  };

  # Graphical applications I want on every desktop machine. Anything heavy or
  # specific to one or two hosts (slicers, calibre, gimp) belongs in that
  # host's home config instead.
  config = lib.mkIf cfg.enable {
    nixpkgs.allowedUnfree = [
      "1password"
      "discord"
      "obsidian"
    ];

    home.packages =
      with pkgs;
      [
        _1password-gui
        firefox
        papers
        pinta
        resources

        unstable.lapce
        unstable.obsidian
        unstable.zed-editor
      ]
      ++ lib.optional discordAvailable discord;
  };
}
