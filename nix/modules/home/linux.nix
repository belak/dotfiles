{ pkgs, lib, ... }:

{
  # All tweaks in this file should be linux-specific and should be commented
  # with the reason.
  home.packages = with pkgs; [
    # GUI apps need to be installed through homebrew on macOS so the .app file
    # shows up in /Applications and is findable by Alfred.
    emacs
    my.wezterm-bin

    # mame-tools is currently broken on macOS, so we keep it Linux-only.
    mame-tools

    # There's no reason to install powertop on macOS because it's focused on
    # optimizing the Linux kernel.
    powertop

    # Terminus needs to be installed from a homebrew cask on macOS to pick it
    # up.
    terminus_font
  ];
}
