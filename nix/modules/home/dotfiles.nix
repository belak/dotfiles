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
      ".config/alacritty" = mkSource "config/alacritty";
      ".config/belak" = mkSource "config/belak";
      ".config/doom" = mkSource "config/doom";
      ".config/git" = mkSource "config/git";
      ".config/nvim" = mkSource "config/nvim";
      ".config/tmux" = mkSource "config/tmux";
      ".config/wezterm" = mkSource "config/wezterm";

      ".editorconfig" = mkSource "editorconfig";
      ".finicky.js" = mkSource "finicky.js";
      ".vimrc" = mkSource "vimrc";
      ".vim" = mkSource "vim";
      ".zshenv" = mkSource "zshenv";
      ".zshrc" = mkSource "zshrc";
    };
  };
}
