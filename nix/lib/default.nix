{ nixpkgs-nixos
, nixpkgs-darwin
, nixpkgs-unstable
, home-manager
, darwin
, ...
}: rec {
  mkPkgs = system: basePkgs: import basePkgs {
    inherit system;

    config.allowUnfreePredicate = pkg: builtins.elem (basePkgs.lib.getName pkg) [
      "discord"
      "obsidian"
      "skypeforlinux"
    ];

    overlays = [
      (final: prev: {
        unstable = nixpkgs-unstable.legacyPackages.${prev.system};
      })
    ];
  };

  isDarwin = system: nixpkgs-nixos.lib.hasSuffix "-darwin" system;

  mkSystemArgs =
    { hostname
    , username
    , homeDirectory
    , system
    , nixpkgs
    , hmModules
    , extraModules
    }: {
      inherit system;

      pkgs = mkPkgs system nixpkgs;

      modules = [
        # Per-host config file
        ../hosts/${hostname}

        # Ensure home-manager is enabled
        home-manager.nixosModules.home-manager

        {
          # Configure a home directory so home-manager can pick it up.
          users.users.${username}.home = homeDirectory;

          # Declare which modules home-manager will use.
          home-manager.users.${username}.imports = hmModules;

          home-manager.useGlobalPkgs = true;
        }
      ] ++ extraModules;
    };

  # mkNixosSystem is a convenience function for declaring a nixos system,
  # and integrating it with home-manager.
  mkNixosSystem =
    { hostname
    , username ? "belak"
    , system ? "x86_64-linux"
    , nixosModules ? [ ]
    , hmModules ? [ ]
    }: nixpkgs-nixos.lib.nixosSystem (mkSystemArgs {
      hostname = hostname;
      username = username;
      homeDirectory = "/home/${username}";
      system = system;
      nixpkgs = nixpkgs-nixos;
      hmModules = [
        ../modules/dotfiles.nix
        ../home-manager/linux.nix
      ] ++ hmModules;
      extraModules = nixosModules;
    });

  # mkNixosSystem is a convenience function for declaring a nixos system,
  # and integrating it with home-manager.
  mkDarwinSystem =
    { hostname
    , username ? "belak"
    , system ? "aarch64-darwin"
    , darwinModules ? [ ]
    , hmModules ? [ ]
    }: darwin.lib.darwinSystem (mkSystemArgs {
      hostname = hostname;
      username = username;
      homeDirectory = "/Users/${username}";
      system = system;
      nixpkgs = nixpkgs-darwin;
      hmModules = [
        ../modules/dotfiles.nix
        ../home-manager/darwin.nix
      ] ++ hmModules;
      extraModules = darwinModules;
    });

  # mkHome is a convenience function for declaring a home-manager setup with our
  # specific package setup.
  mkHome =
    { username ? "belak"
    , system ? "x86_64-linux"
    , hmModules ? [ ]
    }: home-manager.lib.homeManagerConfiguration {
      pkgs =
        if isDarwin system
        then nixpkgs-darwin.legacyPackages.${system}
        else nixpkgs-nixos.legacyPackages.${system};

      modules = [
        ../modules/dotfiles.nix
      ] ++ hmModules;
    };
}
