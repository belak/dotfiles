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
    libation
    nix-init
    typst
    typstfmt
    tinymist
    senpai
    wezterm
  ];
}
