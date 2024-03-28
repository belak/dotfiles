{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [ vim ];

  homebrew = {
    enable = true;

    # Remove any homebrew apps not installed via nix-darwin along with any
    # relevant config files.
    onActivation.cleanup = "zap";

    taps = [
      "d12frosted/emacs-plus"
      "goreleaser/tap"
      "homebrew/cask-fonts"
    ];

    brews = [
      "cbindgen"
      "findutils"
      "gimme"
      "go"
      "gnu-sed"

      # We need OpenJDK for modern versions of Minecraft
      "openjdk@17"

      # Frustratingly, emacs-plus doesn't properly specify gcc as a build
      # dependency, so without this, the brew install fails.
      "gcc"

      {
        name = "d12frosted/emacs-plus/emacs-plus@29";
        args = [ "with-native-comp" ];
      }
    ];

    casks = [
      "alfred"
      "bartender"
      "discord"
      "finicky"
      "firefox"
      "font-terminus"
      {
        name = "font-source-code-pro";
        args.require_sha = false;
      }
      "intellij-idea-ce"
      "modrinth"
      "monodraw"
      "obsidian"
      "openscad"
      "orcaslicer"
      "postgres-unofficial"
      "prismlauncher"
      "prusaslicer"
      "spotify"
      "wezterm"

      # Stuff I'm migrating away from
      "standard-notes"

      # Stuff I'm trying out.
      "via"
    ];

    masApps = {
      "CARROT Weather" = 993487541;
      "Dark Noise" = 1465439395;
      Parcel = 639968404;
      Reeder = 1529448980;
      Things = 904280696;

      # Stuff I'm trying out
      Mela = 1568924476;
      MusicBox = 1614730313;
    };
  };

  nix.settings = {
    auto-optimise-store = true;
    build-users-group = "nixbld";
    experimental-features = [ "nix-command flakes" ];
    trusted-users = [
      "root"
      "kaleb.elwert"
    ];
    warn-dirty = false;
  };

  programs.zsh = {
    enable = true;

    # Similar to NixOS, the default setup does a bunch of weird things so we
    # disable all of them. This lets us properly set them up in our user-level
    # zshrc.
    promptInit = "";
    enableCompletion = false;
    enableBashCompletion = false;
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
