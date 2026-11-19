{
  self,
  nixpkgs,
  agenix,
  disko,
  home-manager,
  darwin,
  nixos-hardware-t2-fork,
  ...
}:
{
  # mkNixosSystem is a convenience function for declaring a nixos system.
  mkNixosSystem =
    {
      modules,
      homeUsers ? { },
    }:
    nixpkgs.lib.nixosSystem {
      specialArgs = {
        inherit self;
        inherit agenix;
        inherit nixos-hardware-t2-fork;
      };

      modules = [
        self.nixosModules.default
        agenix.nixosModules.default
        disko.nixosModules.disko
        home-manager.nixosModules.default
        { home-manager.users = homeUsers; }
      ]
      ++ modules;
    };

  # mkDarwinSystem is a convenience function for declaring a nix-darwin system.
  mkDarwinSystem =
    {
      modules,
      homeUsers ? { },
    }:
    darwin.lib.darwinSystem {
      # We sometimes turn this on when testing against the nix-darwin master
      # branch. This is used to bypass a confidence check which makes sure
      # you're using a stable nixpkgs with stable nix-darwin.
      #
      #enableNixpkgsReleaseCheck = false;

      specialArgs = {
        inherit self;
        inherit agenix;
      };

      modules = [
        self.darwinModules.default
        agenix.darwinModules.default
        home-manager.darwinModules.default
        { home-manager.users = homeUsers; }
      ]
      ++ modules;
    };

  # mkHome is a convenience function for declaring a home-manager config.
  mkHome =
    {
      modules,
      system,
    }:
    home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};

      extraSpecialArgs = {
        inherit self;
      };

      modules = [
        self.homeModules.default
        agenix.homeManagerModules.default
      ]
      ++ modules;
    };
}
