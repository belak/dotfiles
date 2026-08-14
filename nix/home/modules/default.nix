{
  imports = [
    ./common.nix
    ./nixpkgs.nix

    # Platforms
    ./darwin.nix
    ./linux.nix

    # Feature Modules
    ./dev.nix
    ./dotfiles.nix
    ./emacs.nix
    ./ghostty.nix
    ./gnome.nix
    ./gui.nix
    ./vscode.nix
    ./xfce.nix
  ];
}
