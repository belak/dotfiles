{ pkgs, ... }: {
  home.username = "kaleb.elwert";
  home.homeDirectory = "/Users/kaleb.elwert";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs; [ ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
