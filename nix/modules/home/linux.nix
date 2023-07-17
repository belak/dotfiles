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

    # These packages are focused on linux-only features so there's no point
    # installing them on macOS.
    dmidecode
    powertop

    # Terminus needs to be installed from a homebrew cask on macOS to pick it
    # up.
    terminus_font

    # Dwarf fortress is a terrifyingly large mish-mash of packages which
    # introduce complications on macOS, so we only use it on Linux.
    (dwarf-fortress-packages.dwarf-fortress-full.override {
      #dfVersion = "50.09";
      enableIntro = false;
    })
  ];
}
