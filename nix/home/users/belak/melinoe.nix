{ pkgs, ... }:
{
  belak = {
    dotfiles.enable = true;
    dotfiles.symlink = true;
    dev.enable = true;
    emacs.enable = true;
    ghostty.enable = true;

    # Scratch library for trying config changes before they reach freya.
    beets.enable = true;
    vscode.enable = true;

    # Taskwarrior
    taskwarrior.enable = true;
    # We do not set taskwarrior.sync.* here because that would leak the URL credentials
    # and client ID in the public repo. All sync config goes in ~/.taskrc.local instead.

  };

  nixpkgs.allowedUnfree = [
    "claude-code"
    "discord"
    "obsidian"
    #"spotify"
  ];

  home.packages = with pkgs; [
    llm-agents.claude-code

    discord
    neomutt
    pandoc
    typst
    typstyle
    tinymist

    my.senpai
  ];

  programs.starship = {
    enable = true;
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  home.stateVersion = "25.11";
}
