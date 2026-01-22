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
    "claude-code"
    "discord"
    "obsidian"
    #"spotify"
  ];

  home.packages = with pkgs; [
    discord
    libation
    neomutt
    nix-init
    typst
    typstyle
    tinymist

    my.senpai

    unstable.claude-code
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
