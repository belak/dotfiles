{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.belak.niri;
  noctalia = lib.getExe pkgs.unstable.noctalia-shell;
  lock = "${noctalia} ipc call lockScreen lock";

  noctaliaSpawnLocked = args: {
    allow-when-locked = true;
    action.spawn = [ noctalia ] ++ args;
  };
in
{
  options.belak.niri = {
    enable = lib.mkEnableOption "niri";
  };

  config = lib.mkIf cfg.enable {
    programs.niri.package = pkgs.unstable.niri;

    programs.niri.settings = {
      input = {
        keyboard.numlock = true;

        touchpad = {
          tap = true;
          natural-scroll = true;
        };

        warp-mouse-to-focus.enable = true;
        focus-follows-mouse = {
          enable = true;
          max-scroll-amount = "0%";
        };
      };

      outputs."eDP-1" = {
        scale = 1.0;
      };

      layout = {
        gaps = 4;
        default-column-width = {
          proportion = 0.5;
        };

        focus-ring.enable = false;

        border = {
          enable = true;
          width = 1;
          active.color = "#ffc87f";
          inactive.color = "#505050";
          urgent.color = "#9b0000";
        };
      };

      screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";

      binds = {
        "Mod+Shift+Slash".action.show-hotkey-overlay = [ ];

        # Launchers
        "Mod+Return" = {
          hotkey-overlay.title = "Open a Terminal: ghostty";
          action.spawn = [ "ghostty" ];
        };
        "Mod+Space" = {
          hotkey-overlay.title = "Run an Application";
          action.spawn = [ noctalia "ipc" "call" "launcher" "toggle" ];
        };
        "Mod+Shift+L" = {
          hotkey-overlay.title = "Lock the Screen";
          action.spawn = [ noctalia "ipc" "call" "lockScreen" "lock" ];
        };

        # Volume
        "XF86AudioRaiseVolume" = noctaliaSpawnLocked [ "ipc" "call" "volume" "increase" ];
        "XF86AudioLowerVolume" = noctaliaSpawnLocked [ "ipc" "call" "volume" "decrease" ];
        "XF86AudioMute" = noctaliaSpawnLocked [ "ipc" "call" "volume" "muteOutput" ];
        "XF86AudioMicMute" = noctaliaSpawnLocked [ "ipc" "call" "volume" "muteInput" ];

        # Media
        "XF86AudioPlay" = noctaliaSpawnLocked [ "ipc" "call" "media" "playPause" ];
        "XF86AudioStop" = noctaliaSpawnLocked [ "ipc" "call" "media" "stop" ];
        "XF86AudioPrev" = noctaliaSpawnLocked [ "ipc" "call" "media" "previous" ];
        "XF86AudioNext" = noctaliaSpawnLocked [ "ipc" "call" "media" "next" ];

        # Brightness
        "XF86MonBrightnessUp" = noctaliaSpawnLocked [ "ipc" "call" "brightness" "increase" ];
        "XF86MonBrightnessDown" = noctaliaSpawnLocked [ "ipc" "call" "brightness" "decrease" ];

        # Overview and close
        "Mod+O" = {
          repeat = false;
          action.toggle-overview = [ ];
        };
        "Mod+Q" = {
          repeat = false;
          action.close-window = [ ];
        };

        # Focus
        "Mod+Left".action.focus-column-left = [ ];
        "Mod+Down".action.focus-window-down = [ ];
        "Mod+Up".action.focus-window-up = [ ];
        "Mod+Right".action.focus-column-right = [ ];

        # Move
        "Mod+Ctrl+Left".action.move-column-left = [ ];
        "Mod+Ctrl+Down".action.move-window-down = [ ];
        "Mod+Ctrl+Up".action.move-window-up = [ ];
        "Mod+Ctrl+Right".action.move-column-right = [ ];

        # First/last column
        "Mod+Home".action.focus-column-first = [ ];
        "Mod+End".action.focus-column-last = [ ];
        "Mod+Ctrl+Home".action.move-column-to-first = [ ];
        "Mod+Ctrl+End".action.move-column-to-last = [ ];

        # Monitor focus
        "Mod+Shift+Left".action.focus-monitor-left = [ ];
        "Mod+Shift+Down".action.focus-monitor-down = [ ];
        "Mod+Shift+Up".action.focus-monitor-up = [ ];
        "Mod+Shift+Right".action.focus-monitor-right = [ ];

        # Move to monitor
        "Mod+Shift+Ctrl+Left".action.move-column-to-monitor-left = [ ];
        "Mod+Shift+Ctrl+Down".action.move-column-to-monitor-down = [ ];
        "Mod+Shift+Ctrl+Up".action.move-column-to-monitor-up = [ ];
        "Mod+Shift+Ctrl+Right".action.move-column-to-monitor-right = [ ];

        # Workspace navigation
        "Mod+Page_Down".action.focus-workspace-down = [ ];
        "Mod+Page_Up".action.focus-workspace-up = [ ];
        "Mod+Ctrl+Page_Down".action.move-column-to-workspace-down = [ ];
        "Mod+Ctrl+Page_Up".action.move-column-to-workspace-up = [ ];
        "Mod+Shift+Page_Down".action.move-workspace-down = [ ];
        "Mod+Shift+Page_Up".action.move-workspace-up = [ ];

        # Workspace by index
        "Mod+1".action.focus-workspace = 1;
        "Mod+2".action.focus-workspace = 2;
        "Mod+3".action.focus-workspace = 3;
        "Mod+4".action.focus-workspace = 4;
        "Mod+5".action.focus-workspace = 5;
        "Mod+6".action.focus-workspace = 6;
        "Mod+7".action.focus-workspace = 7;
        "Mod+8".action.focus-workspace = 8;
        "Mod+9".action.focus-workspace = 9;
        "Mod+Shift+1".action.move-column-to-workspace = 1;
        "Mod+Shift+2".action.move-column-to-workspace = 2;
        "Mod+Shift+3".action.move-column-to-workspace = 3;
        "Mod+Shift+4".action.move-column-to-workspace = 4;
        "Mod+Shift+5".action.move-column-to-workspace = 5;
        "Mod+Shift+6".action.move-column-to-workspace = 6;
        "Mod+Shift+7".action.move-column-to-workspace = 7;
        "Mod+Shift+8".action.move-column-to-workspace = 8;
        "Mod+Shift+9".action.move-column-to-workspace = 9;

        # Column management
        "Mod+BracketLeft".action.consume-or-expel-window-left = [ ];
        "Mod+BracketRight".action.consume-or-expel-window-right = [ ];
        "Mod+Comma".action.consume-window-into-column = [ ];
        "Mod+Period".action.expel-window-from-column = [ ];

        # Sizing
        "Mod+R".action.switch-preset-column-width = [ ];
        "Mod+Shift+R".action.switch-preset-window-height = [ ];
        "Mod+Ctrl+R".action.reset-window-height = [ ];
        "Mod+F".action.maximize-column = [ ];
        "Mod+Shift+F".action.fullscreen-window = [ ];
        "Mod+Ctrl+F".action.expand-column-to-available-width = [ ];

        # Centering
        "Mod+C".action.center-column = [ ];
        "Mod+Ctrl+C".action.center-visible-columns = [ ];

        # Width/height adjustments
        "Mod+Minus".action.set-column-width = "-10%";
        "Mod+Equal".action.set-column-width = "+10%";
        "Mod+Shift+Minus".action.set-window-height = "-10%";
        "Mod+Shift+Equal".action.set-window-height = "+10%";

        # Floating
        "Mod+V".action.toggle-window-floating = [ ];
        "Mod+Shift+V".action.switch-focus-between-floating-and-tiling = [ ];

        # Tabbed
        "Mod+W".action.toggle-column-tabbed-display = [ ];

        # Screenshots
        "Print".action.screenshot = [ ];
        "Ctrl+Print".action.screenshot-screen = [ ];
        "Alt+Print".action.screenshot-window = [ ];

        # Misc
        "Mod+Escape" = {
          allow-inhibiting = false;
          action.toggle-keyboard-shortcuts-inhibit = [ ];
        };
        "Mod+Shift+E".action.quit = [ ];
        "Ctrl+Alt+Delete".action.quit = [ ];
        "Mod+Shift+P".action.power-off-monitors = [ ];
      };
    };

    home.packages = with pkgs; [
      unstable.niri
      unstable.noctalia-shell
    ];

    systemd.user.services.noctalia-shell = {
      Unit = {
        Description = "Noctalia Shell";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = noctalia;
        Restart = "on-failure";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    # Idle lock
    services.swayidle = {
      enable = true;
      events = [
        {
          event = "before-sleep";
          command = lock;
        }
        {
          event = "lock";
          command = lock;
        }
      ];
      timeouts = [
        {
          timeout = 300;
          command = lock;
        }
      ];
    };
  };
}
