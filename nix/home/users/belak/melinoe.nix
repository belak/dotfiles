{ pkgs, ... }:
let
  # All builtin beets plugins are already enabled by the nixpkgs derivation, so
  # this only wires up the third-party ones.
  beetsWithPlugins = pkgs.python3Packages.beets.override {
    pluginOverrides = {
      alternatives = {
        enable = true;
        propagatedBuildInputs = [ pkgs.python3Packages.beets-alternatives ];
      };
      bandcamp = {
        enable = true;
        propagatedBuildInputs = [ pkgs.python3Packages.beetcamp ];
      };
      filetote = {
        enable = true;
        propagatedBuildInputs = [ pkgs.python3Packages.beets-filetote ];
      };
    };
  };
in
{
  belak = {
    dotfiles.enable = true;
    dotfiles.symlink = true;
    dev.enable = true;
    emacs.enable = true;
    ghostty.enable = true;
    vscode.enable = true;
  };

  nixpkgs.allowedUnfree = [
    "claude-code"
    "discord"
    "obsidian"
    #"spotify"
  ];

  home.packages = with pkgs; [
    llm-agents.claude-code

    beetsWithPlugins

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
