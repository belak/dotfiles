{
  imports = [
    ./common.nix

    # Platforms
    ./darwin.nix
    ./linux.nix

    # Feature Modules
    ./dev.nix
    ./dotfiles.nix
    ./gnome.nix
    ./gui.nix
  ];
}
