{
  config,
  lib,
  pkgs,
  ...
}:

{
  # Unfortunately we can't set these in the common module without an option
  # because the management policy on my work laptop doesn't work with it
  # as expected.
  system.defaults.screensaver = {
    askForPassword = true;
    askForPasswordDelay = 10;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;
}
