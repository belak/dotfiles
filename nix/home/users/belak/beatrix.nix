{ pkgs, ... }:
{
  belak = {
    dotfiles.enable = true;
    dotfiles.symlink = true;
    dev.enable = true;
    gnome.enable = true;
    gui.enable = true;
  };

  nixpkgs.allowedUnfree = [
    "makemkv"
  ];

  home.packages = with pkgs; [
    makemkv
    handbrake
    my.rubyripper

    flac
    lame
    vorbis-tools
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
