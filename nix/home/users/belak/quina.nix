{ pkgs, ... }:
{
  belak = {
    dotfiles = {
      enable = true;
      symlink = true;
    };
    dev.enable = true;
    emacs.enable = true;
    #gnome.enable = true;
    #gui.enable = true;
    #vscode.enable = true;
  };

  home.packages = with pkgs; [
    ags
    alacritty
    brightnessctl
    foot
    fuzzel
    pavucontrol
    senpai
    swaylock
    waybar
    #wpctl

    niri
  ];
}
