{ config, lib, pkgs, ... }:

let
  cfg = config.belak.taskwarrior;
in
{
  options.belak.taskwarrior = {
    enable = lib.mkEnableOption "taskwarrior";
  };

  config = lib.mkIf cfg.enable {
    programs.taskwarrior = {
      enable = true;
      package = pkgs.taskwarrior3;

      # Include a local, unmanaged taskrc for secrets.
      # Because Nix flakes are evaluated purely, any client ID or sync URL
      # containing basic auth credentials placed in the Nix config would
      # become visible in the public dotfiles repo.
      extraConfig = ''
        include ~/.config/task/taskrc.local
      '';
    };

    home.packages = with pkgs; [
      taskwarrior-tui
    ];
  };
}
