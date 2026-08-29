{
  imports = [
    ./common.nix
    ./nixpkgs.nix

    # Platforms
    ./darwin.nix
    ./linux.nix

    # Feature Modules
    ./apps.nix
    ./dev.nix
    ./dotfiles.nix
    ./emacs.nix
    ./ghostty.nix
    ./gnome.nix
    ./gtk.nix
    ./vscode.nix
  ];
}
