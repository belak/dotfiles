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
  };

  home.packages = with pkgs; [ deploy-rs ];
}
