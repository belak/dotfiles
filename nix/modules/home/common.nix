{ pkgs, ... }: {
  home.packages = with pkgs; [
    neovim
    p7zip
    rnix-lsp
    nodePackages.svgo
    unzip

    # Stuff to try
    bat
    cura
    gitui
    lftp
    sublime-music

    # Fun stuff
    cmatrix
    pipes-rs
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

}
