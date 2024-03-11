{ lib, pkgs, ... }:
let
  inherit (pkgs) stdenv;
in
{
  config = lib.mkIf stdenv.isLinux {
    # All tweaks in this file should be linux-specific and should be commented
    # with the reason.
    home.packages = with pkgs; [
      # These packages break things on macOS, so we only install them in Linux.
      #
      # TODO: this is a dev package and we probably shouldn't install it
      # globally.
      clang

      # Partition editing should only really be done on my Linux boxes.
      exfatprogs
      parted

      # These packages are currently broken on macOS, so we keep them Linux-only.
      mame-tools

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
