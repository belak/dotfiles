{ nixpkgs-unstable, ... }: {
  overrides = (final: prev: {
    my = {
      pyenv = final.callPackage ./pkgs/pyenv.nix { };
      pyenv-virtualenv = final.callPackage ./pkgs/pyenv-virtualenv.nix { };
      rbenv = final.callPackage ./pkgs/rbenv.nix { };
      ruby-build = final.callPackage ./pkgs/ruby-build.nix { };
      trekscii = final.callPackage ./pkgs/trekscii.nix { };
      wezterm-bin = final.callPackage ./pkgs/wezterm-bin.nix { };
    };
  });

  unstable = (final: prev: {
    unstable = nixpkgs-unstable.legacyPackages.${prev.system};
  });
}
