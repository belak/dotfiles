{ pkgs, ... }:
{
  belak = {
    dotfiles = {
      enable = true;
      symlink = true;
    };
    dev.enable = true;
    emacs.enable = true;
    gui.enable = true;
    vscode.enable = true;
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
    swww
    waybar

    niri
    xdg-desktop-portal-gtk
    xwayland-satellite
  ];

  systemd.user.services.swww = {
    Install.WantedBy = [ "niri.service" ];

    Service = {
      ExecStart = "${pkgs.swww}/bin/swww-daemon";
      Restart = "on-failure";
    };

    Unit = {
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
      Requisite = [ "graphical-session.target" ];
    };
  };

  systemd.user.services.waybar = {
    Install.WantedBy = [ "niri.service" ];

    Service = {
      ExecStart = "${pkgs.waybar}/bin/waybar";
      Restart = "on-failure";
    };

    Unit = {
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
      Requisite = [ "graphical-session.target" ];
    };
  };
}
