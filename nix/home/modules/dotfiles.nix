{ config, lib, ... }:
let
  cfg = config.belak.dotfiles;
  mkSource =
    sourceFile:
    if cfg.symlink then
      {
        source = config.lib.file.mkOutOfStoreSymlink config.home.homeDirectory + "/.dotfiles/" + sourceFile;
      }
    else
      {
        source = ../../.. + ("/" + sourceFile);
        recursive = true;
      };
in
{
  options.belak.dotfiles = {
    enable = lib.mkEnableOption "dotfiles";
    symlink = lib.mkEnableOption "dotfiles.symlink";
  };

  config = lib.mkIf cfg.enable {
    home.file = {
      "bin" = mkSource "bin";

      ".config/belak" = mkSource "config/belak";
      ".config/direnv/direnv.toml" = mkSource "config/direnv/direnv.toml";
      ".config/doom" = mkSource "config/doom";
      ".config/emacs" = mkSource "config/emacs";
      ".config/finicky" = mkSource "config/finicky";
      ".config/fish" = mkSource "config/fish";
      ".config/ghostty" = mkSource "config/ghostty";
      ".config/git" = mkSource "config/git";
      ".config/jj/config.toml" = mkSource "config/jj/config.toml";
      ".config/nvim" = mkSource "config/nvim";
      ".config/starship.toml" = mkSource "config/starship.toml";
      ".config/tmux" = mkSource "config/tmux";

      ".editorconfig" = mkSource "editorconfig";
      ".hammerspoon" = mkSource "hammerspoon";
      ".vimrc" = mkSource "vimrc";
      ".zshenv" = mkSource "zshenv";
      ".zshrc" = mkSource "zshrc";
      ".zsh_plugins.txt" = mkSource "zsh_plugins.txt";
    };
  };
}
