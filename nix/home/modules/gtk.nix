{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.belak.gtk;
in
{
  options.belak.gtk = {
    enable = lib.mkEnableOption "gtk";
  };

  # Toolkit settings and font rendering only. Most of the desktop software I
  # use is gtk based, so this is shared by gnome and anything else (xfce and
  # friends) rather than living in a desktop-specific module.
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      monaspace

      # Nerd fonts would normally go here too, so Doom Emacs wouldn't need to
      # download them, but it's an 8Gb package.
      dejavu_fonts
      noto-fonts-color-emoji

      # Wayland clipboard bridge. Not strictly a toolkit concern, but every
      # host that enables this is a graphical Linux host.
      wl-clipboard
    ];

    gtk = {
      enable = true;

      iconTheme = {
        name = "Papirus-Dark";
        package = pkgs.papirus-icon-theme;
      };

      theme = {
        name = "adw-gtk3";
        package = pkgs.adw-gtk3;
      };

      colorScheme = "dark";
    };

    dconf = {
      enable = true;

      settings = {
        "org/gnome/desktop/interface" = {
          font-antialiasing = "rgba";
        };
      };
    };
  };
}
