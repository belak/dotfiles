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
      dconf-editor
      gnome-firmware
      gnome-tweaks
    ];

    dconf = {
      enable = true;

      settings = {
        "org/gnome/desktop/interface" = {
          enable-hot-corners = false;
        };
        "org/gnome/mutter" = {
          #dynamic-workspaces = true;
          edge-tiling = true;
        };
        "org/gnome/shell" = {
          disable-user-extensions = false;
          enabled-extensions = with pkgs.gnomeExtensions; [
            unite.extensionUuid
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
  };
}
