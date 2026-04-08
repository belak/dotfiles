{
  self,
  agenix,
  niri,
  ...
}:
{
  config = {
    home-manager = {
      # In a perfect world we'd enable useGlobalPackages, but because we also need
      # to use the same modules as both a home-manager config and a nix-darwin
      # home-manager config, it just works better to keep them separate.
      #useGlobalPkgs = true;

      useUserPackages = true;

      extraSpecialArgs = {
        inherit self;
      };

      sharedModules = [
        self.homeModules.default
        agenix.homeManagerModules.default

        # Use config rather than default to avoid building niri from source.
        # The config module only provides typed settings and build-time
        # validation.
        niri.homeModules.config
      ];
    };
  };
}
