{ pkgs, config, lib, ... }:

let
  cfg = config.belak.gui;
in
{
  options.belak.gui = {
    enable = lib.mkEnableOption "gui";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      calibre
      cura
      discord
      firefox
      gparted
      obsidian
      pavucontrol
      qbittorrent
      rpi-imager
      skypeforlinux
      standardnotes
      xorg.xhost

      my.trekscii
      my.wezterm-bin

      # We use android-studio for android apps while we're still getting the
      # hang of android dev.
      android-studio

      # We use the pure GTK variant of emacs to get better Wayland support
      emacs29-pgtk

      # Packages I want more up to date
      unstable.prusa-slicer

      # Stuff I'm trying out
      sublime-music
    ];

    # This section is in gui rather than gnome because most of the applications I
    # use are gtk based and it's nice to configure them separately from gnome.
    gtk = {
      enable = true;

      iconTheme = {
        name = "Papirus-Dark";
        package = pkgs.papirus-icon-theme;
      };

      theme.name = "Adwaita-dark";

      gtk3.extraConfig = {
        Settings = ''
          gtk-application-prefer-dark-theme=1
        '';
      };

      gtk4.extraConfig = {
        Settings = ''
          gtk-application-prefer-dark-theme=1
        '';
      };
    };
  };
}
