{ pkgs, ... }:
{
  belak = {
    dotfiles.enable = true;
    dotfiles.symlink = true;
    dev.enable = true;
    gnome.enable = true;
    gui.enable = true;
  };

  nixpkgs.allowedUnfree = [
    "makemkv"
  ];

  home.packages = with pkgs; [
    makemkv
    handbrake
    my.rubyripper
  ];
}
