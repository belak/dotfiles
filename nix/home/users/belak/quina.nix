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

  home.packages = with pkgs; [
    llm-agents.claude-code
    llm-agents.pi
    llm-agents.hermes-agent

    # Heavy or host-specific apps, kept out of belak.apps.
    calibre
    unstable.orca-slicer
    unstable.prusa-slicer
  ];

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "26.05";
}
