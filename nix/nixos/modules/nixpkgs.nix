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
