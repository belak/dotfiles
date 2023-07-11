{
  description = "Home Manager configuration of belak";

  inputs = {
    nixpkgs-nixos.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-23.05-darwin";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };
  };

  outputs = inputs @ { nixpkgs-nixos, nixpkgs-darwin, nixpkgs-unstable, nixos-hardware, darwin, home-manager, ... }:
    let
      lib = {
        isDarwin = system: (builtins.elem system inputs.nixpkgs.lib.platforms.darwin);

        # mkDarwinSystem is a convenience function for declaring a nix-darwin
        # system and integrating it with home-manager.
        mkDarwinSystem =
          { hostname
          , username ? "belak"
          , system ? "aarch64-darwin"
          , extraModules ? [ ]
          }: darwin.lib.darwinSystem {
            inherit system;
            pkgs = nixpkgs-darwin.legacyPackages.${system};

            modules = [
              # Per-host config file
              ./hosts/${hostname}

              # Ensure home-manager is enabled
              home-manager.darwinModules.home-manager

              # Configure home-manager. Note that we need to use users.users to
              # declare the home directory rather than home.homeDirectory so
              # home-manager properly picks it up.
              {
                users.users.${username}.home = "/Users/${username}";
                home-manager.users.${username}.imports = [
                  ./modules/unstable-overlay.nix
                  ./modules/dotfiles.nix
                  ./home-manager/darwin.nix
                ];
              }
            ] ++ extraModules;
          };

        # mkNixosSystem is a convenience function for declaring a nixos system,
        # and integrating it with home-manager.
        mkNixosSystem =
          { hostname
          , username ? "belak"
          , system ? "x86_64-linux"
          , extraModules ? [ ]
          }: nixpkgs-nixos.lib.nixosSystem {
            inherit system;
            pkgs = nixpkgs-nixos.legacyPackages.${system};

            modules = [
              # Per-host config file
              ./hosts/${hostname}

              # Ensure home-manager is enabled
              home-manager.nixosModules.home-manager

              # Configure home-manager. Note that we need to use users.users to
              # declare the home directory rather than home.homeDirectory so
              # home-manager properly picks it up.
              {
                users.users.${username}.home = "/home/${username}";
                home-manager.users.${username}.imports = [
                  ./modules/unstable-overlay.nix
                  ./modules/dotfiles.nix
                  ./home-manager/linux.nix
                ];

                # Note that we also need to pass in nixpkgs-unstable so the
                # unstable-overlay functions as expected.
                home-manager.extraSpecialArgs = {
                  nixpkgs-unstable = nixpkgs-unstable;
                };
              }
            ] ++ extraModules;
          };
      };
    in
    {
      formatter = {
        x86_64-linux = nixpkgs-nixos.legacyPackages.x86_64-linux.nixpkgs-fmt;
        aarch64-darwin = nixpkgs-darwin.legacyPackages.aarch64-darwin.nixpkgs-fmt;
        x86_64-darwin = nixpkgs-darwin.legacyPackages.x86_64-darwin.nixpkgs-fmt;
      };

      nixosConfigurations."zagreus" = lib.mkNixosSystem {
        hostname = "zagreus";
        extraModules = [
          nixos-hardware.nixosModules.lenovo-thinkpad-t14
        ];
      };

      darwinConfigurations."COMP-JY4T0D6C0V" = lib.mkDarwinSystem {
        hostname = "COMP-JY4T0D6C0V";
        username = "kaleb.elwert";
      };

      # There are some things nixos and nix-darwin can't provide; for everything
      # else there's home-manager.
      homeConfigurations."belak" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs-nixos.legacyPackages.x86_64-linux;

        modules = [
          ./modules/unstable-overlay.nix
          ./modules/dotfiles.nix
          ./home-manager/linux.nix
        ];
      };
    };
}
