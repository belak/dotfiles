{ pkgs, ... }:
{
  belak = {
    dotfiles = {
      enable = true;
      symlink = true;
    };

    apps.enable = true;
    dev.enable = true;
    emacs.enable = true;
    ghostty.enable = true;
    gnome.enable = true;
  };

  nixpkgs.allowedUnfree = [
    "claude-code"
    #"spotify"
  ];

  home.packages = with pkgs; [
    llm-agents.claude-code
    #spotify
    my.senpai
  ];

  programs.starship = {
    enable = true;
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  home.stateVersion = "26.05";
}
