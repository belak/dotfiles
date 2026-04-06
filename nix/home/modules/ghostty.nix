{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.ghostty;
in
{
  options.belak.ghostty = {
    enable = lib.mkEnableOption "ghostty";
  };

  config = lib.mkIf cfg.enable {
    programs.ghostty = {
      enable = true;

      # On Darwin, ghostty is installed via homebrew cask.
      package = if pkgs.stdenv.hostPlatform.isDarwin then null else pkgs.unstable.ghostty;

      settings = {
        theme = lib.mkDefault "Modus Vivendi";

        # Reset fonts so we can specify our own, along with fallbacks.
        font-family = lib.mkDefault [
          ""
          "Monaspace Neon"
          "Symbols Nerd Font Mono"
          "monospace"
        ];

        font-size = lib.mkDefault 12;

        # On macOS, this fixes font rendering for non-retina screens.
        # https://github.com/ghostty-org/ghostty/issues/661
        font-thicken = lib.mkDefault true;

        cursor-style-blink = lib.mkDefault false;
        gtk-titlebar-style = lib.mkDefault "tabs";
        macos-icon = lib.mkDefault "blueprint";

        working-directory = lib.mkDefault "home";
        window-inherit-working-directory = lib.mkDefault false;

        keybind = lib.mkDefault [
          "cmd+right=next_tab"
          "cmd+left=previous_tab"
        ];
      };

      themes."Modus Vivendi" = {
        palette = [
          "0=#000000"
          "1=#ff5f59"
          "2=#44bc44"
          "3=#d0bc00"
          "4=#2fafff"
          "5=#feacd0"
          "6=#00d3d0"
          "7=#ffffff"
          "8=#1e1e1e"
          "9=#ff5f5f"
          "10=#44df44"
          "11=#efef00"
          "12=#338fff"
          "13=#ff66ff"
          "14=#00eff0"
          "15=#989898"
        ];

        background = "000000";
        foreground = "ffffff";
        cursor-color = "ffffff";
        cursor-text = "000000";
        selection-background = "7030af";
        selection-foreground = "ffffff";
      };
    };
  };
}
