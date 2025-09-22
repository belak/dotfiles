{ pkgs, ... }:
{
  belak = {
    dotfiles = {
      enable = true;
      symlink = true;
    };
    dev.enable = true;
    emacs.enable = true;
    gnome.enable = true;
    gui.enable = true;
    vscode.enable = true;
  };

  home.packages = with pkgs; [
    eww
    foot
    fuzzel
    kitty
    niri
    #niriswitcher
    river
    senpai
    swaylock
    uwsm
    wofi
    xwayland-satellite
  ];
}
