{
  config,
  lib,
  pkgs,
  ...
}:

{
  system.primaryUser = "belak";

  # Unfortunately we can't set these in the common module without an option
  # because the management policy on my work laptop doesn't work with it
  # as expected.
  system.defaults.screensaver = {
    askForPassword = true;
    # TODO: this doesn't seem to work, so we have to set it manually
    askForPasswordDelay = 5;
  };

  homebrew = {
    casks = [
      "blackhole-2ch"
      "claude-code"
      "inkscape"
      "meta"
    ];

    masApps = {
      "Logic Pro" = 634148309;
    };
  };

  programs.fish = {
    enable = true;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;
}
