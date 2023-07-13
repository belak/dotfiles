{
  packages = (final: prev: {
    # Overrides
    my = { };

    # Additional packages
    wezterm-bin = final.callPackage ./packages/wezterm-bin.nix { };
  });
}
