# Near-copies of this file live in nix/darwin/modules, nix/home/modules and
# nix/nixos/modules. They are identical today, but each one configures a
# different module system, so they are kept separate rather than shared. Check
# the other two when changing this one.
{
  self,
  config,
  lib,
  ...
}:
let
  allowed = config.nixpkgs.allowedUnfree;
in
{
  options.nixpkgs = {
    allowedUnfree = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        Allows for unfree packages by their name.
      '';
    };
  };

  config = {
    nixpkgs.overlays = builtins.attrValues self.overlays;
    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) allowed;
  };
}
