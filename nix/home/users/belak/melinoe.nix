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
    senpai

    # The Spotify package is currently broken, so we install it using homebrew
    # instead.
    #spotify
    templ

    unstable.claude-code
  ];

  programs.starship = {
    enable = true;
  };
}
