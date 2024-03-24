{ config, lib, ... }:
let
  cfg = config.belak.dotfiles;
in
{
  options.belak.dotfiles = {
    enable = lib.mkEnableOption "dotfiles";
  };

  config = lib.mkIf cfg.enable {
    home.file = {
      ".config" = {
        source = ../../../config;
        recursive = true;
      };
      ".vim" = {
        source = ../../../vim;
        recursive = true;
      };
      ".editorconfig".source = ../../../editorconfig;
      ".finicky.js".source = ../../../finicky.js;
      ".vimrc".source = ../../../vimrc;
      ".zshenv".source = ../../../zshenv;
      ".zshrc".source = ../../../zshrc;
    };
  };
}
