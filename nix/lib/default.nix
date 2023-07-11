{ nixpkgs-nixos
, nixpkgs-darwin
, nixpkgs-unstable
, home-manager
, darwin
, ...
}: rec {
  isDarwin = system: nixpkgs-nixos.lib.hasSuffix "-darwin" system;

  mkSystemArgs =
    { hostname
    , username
    , system
    , nixpkgs
    , hmModules
    , extraModules
    }: {
      inherit system;

      # We bolt on nixpkgs-unstable here so it can be used in system-wide
      # modules.
      pkgs =
        (if isDarwin system
        then nixpkgs-darwin.legacyPackages.${system}
        else nixpkgs-nixos.legacyPackages.${system}) // {
          unstable = nixpkgs-unstable.legacyPackages.${system};
        };

      modules = [
        # Per-host config file
        ../hosts/${hostname}

        # Ensure home-manager is enabled
        home-manager.nixosModules.home-manager

        # Configure home-manager. Note that we need to use users.users to
        # declare the home directory rather than home.homeDirectory so
        # home-manager properly picks it up.
        {
          users.users.${username}.home =
            if isDarwin system
            then "/Users/${username}"
            else "/home/${username}";

          home-manager.users.${username}.imports = hmModules;

          # Note that we also need to pass in nixpkgs-unstable so the
          # unstable-overlay functions as expected.
          home-manager.extraSpecialArgs = {
            nixpkgs-unstable = nixpkgs-unstable;
          };
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
      system = system;
      nixpkgs = nixpkgs-nixos;
      hmModules = [
        ../modules/unstable-overlay.nix
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
      system = system;
      nixpkgs = nixpkgs-nixos;
      hmModules = [
        ../modules/unstable-overlay.nix
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
        ./modules/unstable-overlay.nix
        ./modules/dotfiles.nix
      ] ++ hmModules;

      extraSpecialArgs = {
        nixpkgs-unstable = nixpkgs-unstable;
      };
    };
}
