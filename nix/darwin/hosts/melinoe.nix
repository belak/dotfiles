{
  self,
  agenix,
  config,
  lib,
  pkgs,
  ...
}:

{
  nixpkgs.hostPlatform = lib.mkDefault "aarch64-darwin";

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
    brews = [
      "ccusage"
    ];

    casks = [
      "adobe-digital-editions"
      "blackhole-2ch"
      "inkscape"
      "spotify"
      "claude-code"
    ];

    masApps = {
      "Logic Pro" = 634148309;
      "Meta" = 558317092;
      "Mp3tag" = 1532597159;
    };
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;
}
