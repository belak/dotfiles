{
  config,
  lib,
  ...
}:
let
  cfg = config.belak.cosmic;
in
{
  options.belak.cosmic = {
    enable = lib.mkEnableOption "cosmic";
  };

  config = lib.mkIf cfg.enable {
    # If cosmic is enabled, we want to make sure we enable the provided cache.
    nix.settings = {
      substituters = [ "https://cosmic.cachix.org/" ];
      trusted-public-keys = [ "cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE=" ];
    };

    services.desktopManager.cosmic.enable = true;
    services.displayManager.cosmic-greeter.enable = true;
  };
}
