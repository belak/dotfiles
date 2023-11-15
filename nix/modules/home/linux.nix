{ lib, pkgs, ... }:

let
  inherit (pkgs) stdenv;
in
{
  config = lib.mkIf stdenv.isLinux {
    # All tweaks in this file should be linux-specific and should be commented
    # with the reason.
    home.packages = with pkgs; [
      # These packages are currently broken on macOS, so we keep them Linux-only.
      mame-tools
      neomutt

      # These packages are focused on linux-only features so there's no point
      # installing them on macOS.
      dmidecode
      powertop

      # Terminus needs to be installed from a homebrew cask on macOS to pick it
      # up.
      terminus_font

      # Various additional fonts which improve the Linux experience
      dejavu_fonts
      noto-fonts-emoji
    ];
  };
}
