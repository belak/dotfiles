{nixpkgs-unstable, ...}: {
  overrides = final: prev: {
    my = {
      pyenv = final.callPackage ./pkgs/pyenv.nix {};
      pyenv-virtualenv = final.callPackage ./pkgs/pyenv-virtualenv.nix {};
      rbenv = final.callPackage ./pkgs/rbenv.nix {};
      ruby-build = final.callPackage ./pkgs/ruby-build.nix {};
      trekscii = final.callPackage ./pkgs/trekscii.nix {};
      wezterm-bin = final.callPackage ./pkgs/wezterm-bin.nix {};
    };
  };

  unstable = final: prev: {
    # Normally this would be nixpkgs-unstable.legacyPackages.${prev.system}, but
    # we need to set config.allowUnfreePredicate, so we build in a minimal
    # version of lib.mkPkgs.
    unstable = import nixpkgs-unstable {
      inherit (prev) system;

      config.allowUnfreePredicate = pkg:
        builtins.elem (nixpkgs-unstable.lib.getName pkg) [
          "obsidian"
        ];

      config.permittedInsecurePackages = [
        "electron-25.9.0"
      ];
    };
  };
}
