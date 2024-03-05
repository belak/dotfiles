{ nixpkgs-unstable, ... }:
{
  additions = final: _prev: { my = import ./pkgs { pkgs = final; }; };

  unstable = final: _prev: {
    # Normally this would be nixpkgs-unstable.legacyPackages.${prev.system}, but
    # we need to set config.allowUnfreePredicate, so we build in a minimal
    # version of lib.mkPkgs.
    unstable = import nixpkgs-unstable {
      inherit (final) system;

      config.allowUnfreePredicate = pkg: builtins.elem (final.lib.getName pkg) [ "obsidian" ];

      config.permittedInsecurePackages = [ "electron-25.9.0" ];
    };
  };
}
