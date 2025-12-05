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
    mako
    pavucontrol
    senpai
    swww
    walker
    waybar
    xdg-utils

    xdg-desktop-portal-gtk
    xwayland-satellite

    unstable.niri
  ];

  systemd.user.services.mako = {
    Install.WantedBy = [ "niri.service" ];

    Service = {
      ExecStart = "${pkgs.mako}/bin/mako";
      Restart = "on-failure";
    };

    Unit = {
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
      Requisite = [ "graphical-session.target" ];
    };
  };

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

  systemd.user.services.swayidle = {
    Install.WantedBy = [ "niri.service" ];

    Service = {
      ExecStart = "${pkgs.swayidle}/bin/swayidle -w";
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
