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
    "spotify"
  ];

  home.packages = with pkgs; [
    discord
    libation
    neomutt
    nix-init
    typst
    typstfmt
    tinymist
    senpai
    spotify
    templ
  ];

  programs.starship = {
    enable = true;
  };
}
