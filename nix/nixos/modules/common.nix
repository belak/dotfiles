{ lib, pkgs, ... }:
{
  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';

    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      auto-optimise-store = true;
      warn-dirty = false;
    };

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 2d";
    };
  };

  users.mutableUsers = false;

  environment.systemPackages = with pkgs; [
    dig
    git
    sysbench
    vim

    ghostty.terminfo
    tmux.terminfo
  ];

  # This installs some extra TERMINFO files
  #environment.enableAllTerminfo = true;

  i18n.defaultLocale = "en_US.UTF-8";

  # Default to US Pacific, but allow it to be overridden, such as in server.nix
  time.timeZone = lib.mkDefault "US/Pacific";

  programs.fish = {
    enable = true;
  };

  programs.zsh = {
    enable = true;

    promptInit = "";
    setOptions = [ ];
    enableGlobalCompInit = false;
  };
}
