{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    editorconfig-core-c
    fd
    neovim
    ripgrep
    tmux
    vim
  ];

  homebrew = {
    enable = true;

    taps = [
      "d12frosted/emacs-plus"
      "goreleaser/tap"
      "homebrew/cask-fonts"
    ];

    brews = [
      "cbindgen"
      "cloc"
      "dos2unix"
      "findutils"
      "gimme"
      "git"
      "gitui"
      "go"
      "golangci-lint"
      "goreleaser"
      "gnu-sed"
      "htop"
      "lftp"
      "p7zip"
      "protoc-gen-go"
      "protoc-gen-go-grpc"
      "pwgen"
      "sdl12-compat"
      "sdl_gfx"
      "sdl_image"
      "sdl_mixer"
      "sdl_ttf"
      "svgo"
      "wget"
    ];

    casks = [
      "alfred"
      "bartender"
      "discord"
      "finicky"
      "firefox"
      "font-terminus"
      { name = "font-source-code-pro"; args.require_sha = false; }
      "monodraw"
      "obsidian"
      "postgres-unofficial"
      "prusaslicer"
      "trailer"
      "wezterm"
    ];

    masApps = {
      Parcel = 639968404;
      Reeder = 1529448980;
      Things = 904280696;
    };
  };

  nix.settings = {
    auto-optimise-store = true;
    build-users-group = "nixbld";
    experimental-features = "nix-command flakes";
    trusted-users = [ "root" "kaleb.elwert" ];
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
