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
    "obsidian"
  ];

  home.packages = with pkgs; [
    ghostty
    libation
    nix-init
    typst
    typstyle
    tinymist
    senpai
  ];
}
