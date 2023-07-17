{ pkgs, ... }: {
  home.packages = with pkgs; [
    binutils
    curl
    dig
    fd
    findutils
    fzf
    git
    htop
    isync
    jq
    killall
    mu
    p7zip
    pwgen
    ripgrep
    rnix-lsp
    nodePackages.svgo
    tmux
    unzip
    vim
    wget

    # Stuff to try
    bat
    gitui
    lftp
    neomutt
    neovim

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
