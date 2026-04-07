{ pkgs, ... }:
{
  belak = {
    dotfiles = {
      enable = true;
      symlink = true;
    };
    dev.enable = true;
    emacs.enable = true;
    ghostty.enable = true;
    gui.enable = true;
    niri.enable = true;
    vscode.enable = true;
    #xfce.enable = true;
  };

  programs.ghostty.settings = {
    window-decoration = "none";
    gtk-titlebar-style = "native";
  };

  nixpkgs.allowedUnfree = [
    "claude-code"
  ];

  home.packages = with pkgs; [
    pavucontrol
    senpai
    yubioath-flutter

    claude-code
  ];

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11";
}
