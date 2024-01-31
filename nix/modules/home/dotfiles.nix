{ config, lib, ... }:
let
  cfg = config.belak.dotfiles;
in
{
  options.belak.dotfiles = {
    enable = lib.mkEnableOption "dotfiles";
    flakePath = lib.mkOption { default = "${config.belak.homeDirectory}/.dotfiles"; };
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

      # Set up this file to be symlinked into the location home-manager expects it
      # to be. This allows us to set it up once by passing `-f` and not have to
      # worry about it again.
      #
      # Note that we have to use mkOutOfStoreSymlink because we want `home.nix` to
      # be a symlink rather than a file in the nix store. Putting it in the nix
      # store causes makes it so you have to run `home-manager switch` twice for
      # every change to `home.nix` (the first update causes the file to update,
      # the second actually uses it), and makes it harder to recover from syntax
      # errors.
      ".config/home-manager/flake.nix".source = config.lib.file.mkOutOfStoreSymlink "${cfg.flakePath}/flake.nix";
    };
  };
}
