{ nixpkgs-unstable, ... }: {
  overrides = (final: prev: {
    my = { };
  });

  extras = (final: prev: {
    wezterm-bin = final.callPackage ./pkgs/wezterm-bin.nix { };
  });

  unstable = (final: prev: {
    unstable = nixpkgs-unstable.legacyPackages.${prev.system};
  });
}
