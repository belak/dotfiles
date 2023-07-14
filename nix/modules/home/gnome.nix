{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    gnome-firmware
    gnome.dconf-editor
    gnomeExtensions.dash-to-dock
    gnomeExtensions.space-bar
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
      dynamic-workspaces = true;
      edge-tiling = true;
    };
    "org/gnome/shell" = {
      enabled-extensions = [
        "unite@hardpixel.eu"
        "dash-to-dock@micxgx.gmail.com"
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "space-bar@luchrioh"
      ];
      favorite-apps = [
        "firefox.desktop"
        "org.wezfurlong.wezterm.desktop"
        "discord.desktop"
      ];
    };
    "org/gnome/shell/extensions/dash-to-dock" = {
      apply-custom-theme = false;
      background-opacity = 0.0;
      dash-max-icon-size = 48;
      hot-keys = false;
      show-show-apps-button = false;
      show-trash = false;
      transparency-mode = "FIXED";
    };
    "org/gnome/shell/extensions/space-bar/appearance" = {
      workspaces-bar-padding = 5;
    };
    "org/gnome/shell/extensions/space-bar/shortcuts" = {
      enable-activate-workspace-shortcuts = false;
      enable-move-to-workspace-shortcuts = false;
    };
    "org/gnome/shell/extensions/unite" = {
      hide-activities-button = "always";
      hide-window-titlebars = "both";
      show-window-buttons = "both";
      show-window-title = "both";
    };
    "org/gnome/tweaks" = {
      show-extensions-notice = false;
    };
  };

  dconf.settings."org/gnome/desktop/wm/keybindings" = lib.listToAttrs (lib.concatLists (map
    (n: [
      { name = "move-to-workspace-${toString n}"; value = [ "<Super><Shift>${toString n}" ]; }
      { name = "switch-to-workspace-${toString n}"; value = [ "<Super>${toString n}" ]; }
    ])
    (lib.range 1 9)));

  dconf.settings."org/gnome/shell/keybindings" = lib.listToAttrs (lib.concatLists (map
    (n: [
      { name = "switch-to-application-${toString n}"; value = [ ]; }
    ])
    (lib.range 1 9)));
}
