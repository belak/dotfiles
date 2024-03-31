{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.belak.gnome;
in
{
  options.belak.gnome = {
    enable = lib.mkEnableOption "gnome";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      gnome-firmware
      gnome.dconf-editor
      gnome.gnome-tweaks
      gnomeExtensions.unite
    ];

    dconf.settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
        enable-hot-corners = false;
        font-antialiasing = "rgba";
        gtk-theme = "Adwaita-dark";
      };
      "org/gnome/mutter" = {
        #dynamic-workspaces = true;
        edge-tiling = true;
      };
      "org/gnome/shell" = {
        enabled-extensions = [
          "unite@hardpixel.eu"
          "user-theme@gnome-shell-extensions.gcampax.github.com"
        ];
        favorite-apps = [
          "firefox.desktop"
          "org.wezfurlong.wezterm.desktop"
          "discord.desktop"
        ];
      };
      "org/gnome/shell/extensions/unite" = {
        hide-activities-button = "never";
        hide-window-titlebars = "both";
        reduce-panel-spacing = false;
        show-window-buttons = "both";
        show-window-title = "both";
        use-activities-text = false;
      };
      "org/gnome/tweaks" = {
        show-extensions-notice = false;
      };

      # Keybinds
      "org/gnome/desktop/wm/keybindings" = lib.listToAttrs (
        lib.concatLists (
          map (n: [
            {
              name = "move-to-workspace-${toString n}";
              value = [ "<Super><Shift>${toString n}" ];
            }
            {
              name = "switch-to-workspace-${toString n}";
              value = [ "<Super>${toString n}" ];
            }
          ]) (lib.range 1 9)
        )
      );

      "org/gnome/shell/keybindings" = lib.listToAttrs (
        lib.concatLists (
          map (n: [
            {
              name = "switch-to-application-${toString n}";
              value = [ ];
            }
          ]) (lib.range 1 9)
        )
      );
    };
  };
}
