{ pkgs, ... }:
{
  belak = {
    dotfiles.enable = true;
    dotfiles.symlink = true;
    dev.enable = true;
    emacs.enable = true;
    vscode.enable = true;
  };

  nixpkgs.allowedUnfree = [
    "discord"
    "obsidian"
    #"spotify"
  ];

  home.packages = with pkgs; [
    discord
    libation
    neomutt
    nix-init
    pandoc
    typst
    typstyle
    tinymist

    my.senpai
  ];

  programs.starship = {
    enable = true;
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  home.stateVersion = "25.11";
}
