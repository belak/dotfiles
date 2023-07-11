{ nixpkgs-unstable, ... }: {
  # Adding a simple overlay which tacks on the unstable branch of nixpkgs allows
  # us to reference any unstable versions of packages just by specifying
  # pkgs.unstable rather than having a separate import for any modules which use
  # it.
  nixpkgs.overlays = [
    (final: prev: {
      unstable = nixpkgs-unstable.legacyPackages.${prev.system};
    })
  ];
}
