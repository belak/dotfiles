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
  #
  # On macOS anything with its own update channel or deep OS integration comes
  # from a homebrew cask instead, because a read-only store path can't self
  # update and can't be code-signed into the OS the way those apps expect. The
  # shared list is what nix packages just as well as a cask would.
  config = lib.mkIf cfg.enable {
    nixpkgs.allowedUnfree = [
      "1password"
      "discord"
      "obsidian"
    ];

    home.packages =
      with pkgs;
      [
        unstable.lapce
        unstable.lite-xl
        unstable.obsidian
        unstable.zed-editor
      ]
      ++ lib.optionals pkgs.stdenv.isLinux [
        # These packages are available as both homebrew casks and nix packages,
        # but we prefer homebrew for various reasons.
        _1password-gui
        firefox

        # These packages either have dependencies that don't work well on macOS
        # or are linux-only by nature.
        gparted
        papers
        pinta
        resources
        unstable.textadept
      ]
      # Discord refuses to launch until it has updated itself, which a
      # read-only store path can't do.
      ++ lib.optional (pkgs.stdenv.isLinux && discordAvailable) discord;
  };
}
