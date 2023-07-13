{ self
, nixpkgs-nixos
, nixpkgs-darwin
, home-manager
, darwin
, ...
} @ inputs: rec {
  mkPkgs = system: nixpkgs: import nixpkgs {
    inherit system;

    # It's easiest to configure our unfree packages for every nixpkgs input
    # rather than on a system-by-system basis.
    config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [
      "discord"
      "obsidian"
      "skypeforlinux"
    ];

    overlays = builtins.attrValues self.overlays;
  };

  isDarwin = system: builtins.elem system nixpkgs-darwin.lib.platforms.darwin;

  # mkNixosSystem is a convenience function for declaring a nixos system,
  # and integrating it with home-manager.
  mkNixosSystem =
    { hostname
    , username ? "belak"
    , system ? "x86_64-linux"
    , nixosModules ? [ ]
    , hmModules ? [ ]
    }:
    nixpkgs-nixos.lib.nixosSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs-nixos;

      modules = [
        ./hosts/nixos/${hostname}
        home-manager.nixosModules.home-manager

        # Configure home-manager and set up a user so home-manager can pick up
        # the homeDirectory.
        {
          home-manager.useGlobalPkgs = true;
          users.users.${username}.home = "/home/${username}";
          home-manager.users.${username}.imports = hmModules;
        }
      ] ++ nixosModules;
    };

  # mkDarwinSystem is a convenience function for declaring a nix-darwin system,
  # and integrating it with home-manager.
  mkDarwinSystem =
    { hostname
    , username ? "belak"
    , system ? "aarch64-darwin"
    , darwinModules ? [ ]
    , hmModules ? [ ]
    }:
    darwin.lib.darwinSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs-darwin;

      modules = [
        ./hosts/darwin/${hostname}
        home-manager.darwinModules.home-manager

        # Configure home-manager and set up a user so home-manager can pick up
        # the homeDirectory.
        {
          home-manager.useGlobalPkgs = true;
          users.users.${username}.home = "/Users/${username}";
          home-manager.users.${username}.imports = hmModules;
        }
      ] ++ darwinModules;
    };

  # mkHome is a convenience function for declaring a home-manager setup with our
  # specific package setup.
  mkHome =
    { username ? "belak"
    , system ? "x86_64-linux"
    , hmModules ? [ ]
    }: home-manager.lib.homeManagerConfiguration {
      pkgs = mkPkgs system
        (if isDarwin system
        then nixpkgs-darwin
        else nixpkgs-nixos);

      modules = hmModules;
    };
}
