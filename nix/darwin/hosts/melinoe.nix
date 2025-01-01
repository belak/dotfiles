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
    # TODO: this doesn't seem to work, so we have to set it manually
    askForPasswordDelay = 5;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;
}
