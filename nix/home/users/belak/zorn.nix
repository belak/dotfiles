{ pkgs, ... }:
{
  belak = {
    dotfiles = {
      enable = true;
      symlink = true;
    };
    dev.enable = true;
    gnome.enable = true;
    gui.enable = true;
  };

  nixpkgs.allowedUnfree = [
    "1password"
    "spotify"
  ];

  home.packages = with pkgs; [
    _1password-gui
    deploy-rs
    solaar
    spotify
  ];
}
