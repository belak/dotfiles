{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    mas
    vim
  ];

  homebrew = {
    enable = true;

    # Remove any homebrew apps not installed via nix-darwin along with any
    # relevant config files.
    onActivation.cleanup = "zap";

    brews = [
      "cbindgen"
      "findutils"
      "gimme"
      "go"
      "gnu-sed"

      # We need OpenJDK for modern versions of Minecraft
      "openjdk@17"
    ];

    casks = [
      "1password"
      "1password-cli"
      "airbuddy"
      "alfred"
      "calibre"
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
      "jordanbaird-ice@beta" # TODO: we need to use "Beta" for now to support Tahoe.
      "intellij-idea-ce"
      "modrinth"
      "monodraw"
      "mos"
      "obsidian"
      "openscad"
      "orcaslicer"
      "pinta"
      "postgres-unofficial"
      "prismlauncher"
      "prusaslicer"
      "rectangle"
      "textual"
      "wezterm"

      # Stuff I'm trying out.
      "hammerspoon"
      "utm"
      "zotero"
    ];

    masApps = {
      "CARROT Weather" = 993487541;
      "Dark Noise" = 1465439395;
      "Parcel Classic" = 639968404;
      Reeder = 1529448980;
      Things = 904280696;
      Xcode = 497799835;

      # Stuff I'm trying out
      MusicBox = 1614730313;
    };
  };

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
      tilesize = 48;
      persistent-apps = [
        "/Applications/Firefox.app"
        "/Applications/Ghostty.app"
        "/Applications/Obsidian.app"
        "/Applications/Things3.app"
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
