{ lib, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    mas
    vim
  ];

  homebrew = {
    enable = true;

    onActivation = {
      # Remove any homebrew apps not installed via nix-darwin along with any
      # relevant config files.
      cleanup = "zap";

      # Required since Homebrew e0d818b; brew bundle --cleanup now needs explicit
      # confirmation. Remove once nix-darwin#1789 lands.
      extraFlags = [ "--force-cleanup" ];
    };

    brews = [
      "cbindgen"
      "findutils"
      "gimme"
      "go"
      "gnu-sed"

      # We need OpenJDK for modern versions of Minecraft
      "openjdk@17"
      "openjdk@21"
    ];

    casks = [
      "1password"
      "airbuddy"
      "alfred"
      "calibre"

      # Discord refuses to launch until it has updated itself, which a
      # read-only store path can't do, so it comes from a cask on macOS.
      "discord"

      "finicky"
      "firefox"
      "font-jetbrains-mono"
      "font-monaspace"
      "font-terminus"
      {
        name = "font-source-code-pro";
        args.require_sha = false;
      }
      "font-symbols-only-nerd-font"
      "ghostty"
      "hammerspoon"
      "mediamate"
      "modrinth"
      "monodraw"
      "mos"
      "openscad"
      "orcaslicer"
      "postgres-app"
      "prismlauncher"
      "prusaslicer"
      "soundsource"
      "textual"
      "thaw"

      # Stuff I'm trying out.
      "pinta"
      "sol"
      "utm"
      "zotero"
    ];

    # Disabled until nix-darwin/nix-darwin#1668 lands — current mas
    # re-installs every app on each activation, which is painful for large
    # apps like Xcode. Flip to `true` during bootstrapping when we actually
    # want these reconciled.
    masApps = lib.mkIf false {
      "CARROT Weather" = 993487541;
      "Dark Noise" = 1465439395;
      MediaInfo = 510620098;
      Parcel = 375589283;
      Reeder = 1529448980;
      Things = 904280696;
      Todoist = 585829637;
      Xcode = 497799835;

      # Stuff I'm trying out
      MusicBox = 1614730313;
    };
  };

  environment.shells = with pkgs; [
    fish
    zsh
  ];

  # Until https://github.com/NixOS/nix/issues/7273 is fixed,
  # auto-optimize-store should be left off. We can approximate it by using
  # nix.optimize.automatic to run `nix store optimize` on a schedule.
  # nix.settings.auto-optimize-store = true;
  nix.optimise.automatic = true;

  nix.settings = {
    build-users-group = "nixbld";
    experimental-features = [ "nix-command flakes" ];
    trusted-users = [
      "root"
      "belak"
      "kaleb.elwert"
    ];
    warn-dirty = false;
  };

  system.defaults = {
    dock = {
      autohide = true;
      show-recents = false;
      minimize-to-application = true;
      mru-spaces = false;
      tilesize = 48;
      # The basic apps I need to launch when something else is broken.
      # Everything else goes through a launcher. Both of these are casks from
      # this module, so /Applications is the right path; nix-installed apps
      # land elsewhere.
      persistent-apps = [
        "/Applications/Firefox.app"
        "/Applications/Ghostty.app"
      ];
    };

    finder = {
      ShowPathbar = true;
      _FXSortFoldersFirst = true;
    };

    #loginwindow.SHOWFULLNAME = true;

    screencapture.disable-shadow = true;

    # Disable built-in macOS tiling, as we use Rectangle to provide more options
    # when tiling windows.
    WindowManager = {
      EnableTilingByEdgeDrag = false;
      EnableTopTilingByEdgeDrag = false;
      EnableTilingOptionAccelerator = false;
      EnableTiledWindowMargins = false;
    };

    NSGlobalDomain = {
      NSAutomaticDashSubstitutionEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
      NSAutomaticQuoteSubstitutionEnabled = false;

      NSWindowShouldDragOnGesture = true;

      AppleInterfaceStyle = "Dark";
    };

    CustomUserPreferences = {
      "com.apple.desktopservices" = {
        "DSDontWriteNetworkStores" = true;
        "DSDontWriteUSBStores" = true;
      };
    };
  };

  system.startup.chime = false;

  #keyboard.remapCapsLockToEscape = true;

  programs.fish = {
    enable = true;
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
}
