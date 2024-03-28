{ pkgs, ... }:
{
  belak = {
    dotfiles.enable = true;
    dev.enable = true;
    gnome.enable = true;
    gui.enable = true;
  };

  home.packages = with pkgs; [ colmena ];
}
