{ lib, pkgs, ... }:
let
  inherit (pkgs) stdenv;
in
{
  config = lib.mkIf stdenv.isLinux {
    # All tweaks in this file should be linux-specific and should be commented
    # with the reason.
    home.packages = with pkgs; [
      # Partition editing should only really be done on my Linux boxes.
      exfatprogs
      parted

      # These packages are currently broken on macOS, so we keep them Linux-only.
      mame-tools
      ncdu

      # These packages are focused on linux-only features so there's no point
      # installing them on macOS.
      dmidecode
      powertop

      # Terminus needs to be installed from a homebrew cask on macOS to pick it
      # up so we install it for Linux here.
      terminus_font

      # Various additional fonts which improve the Linux experience
      dejavu_fonts
      nerdfonts
      noto-fonts-emoji
    ];
  };
}
