{ pkgs, ... }: {
  home.packages = with pkgs; [
    calibre
    cura
    discord
    firefox
    obsidian
    pavucontrol
    rpi-imager
    skypeforlinux
    standardnotes

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
}
