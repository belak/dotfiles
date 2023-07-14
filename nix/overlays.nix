{ nixpkgs-unstable, ... }: {
  overrides = (final: prev: {
    my = {
      pyenv = final.callPackage ./pkgs/pyenv.nix { };
      pyenv-virtualenv = final.callPackage ./pkgs/pyenv-virtualenv.nix { };
      wezterm-bin = final.callPackage ./pkgs/wezterm-bin.nix { };
    };
  });

  unstable = (final: prev: {
    unstable = nixpkgs-unstable.legacyPackages.${prev.system};
  });
}
