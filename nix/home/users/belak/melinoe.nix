{ pkgs, ... }:
{
  belak = {
    dotfiles.enable = true;
    dotfiles.symlink = true;
    dev.enable = true;
  };

  nixpkgs.allowedUnfree = [
    "discord"
    "obsidian"
    "spotify"
  ];

  home.packages = with pkgs; [
    deploy-rs
    discord
    typst
    typstfmt
    typst-preview
    spotify
  ];
}
