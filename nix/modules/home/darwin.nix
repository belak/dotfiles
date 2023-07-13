{ pkgs, ... }: {
  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs; [
    cloc
    crane
    curl
    dig
    dos2unix
    editorconfig-core-c
    fd
    fzf
    git
    htop
    jq
    #mame-tools
    neovim
    nodePackages.svgo
    p7zip
    pwgen
    ripgrep
    tmux
    unzip
    wget

    # Stuff to try
    bat
    gitui
    ko
    lftp
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
