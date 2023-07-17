{ lib, pkgs, ... }:

let
  inherit (pkgs) stdenv;
in
{
  config = lib.mkIf stdenv.isLinux {
    # All tweaks in this file should be linux-specific and should be commented
    # with the reason.
    home.packages = with pkgs; [
      # mame-tools is currently broken on macOS, so we keep it Linux-only.
      mame-tools

      # These packages are focused on linux-only features so there's no point
      # installing them on macOS.
      dmidecode
      powertop

      # Terminus needs to be installed from a homebrew cask on macOS to pick it
      # up.
      terminus_font
    ];
  };
}
