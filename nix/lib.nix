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
      "dwarf-fortress"
      "obsidian"
      "skypeforlinux"
    ];

    overlays = builtins.attrValues self.overlays;
  };

  optionalPath = path: if (builtins.pathExists path) then [ path ] else [ ];

  isDarwin = system: builtins.elem system nixpkgs-darwin.lib.platforms.darwin;

  isLinux = system: builtins.elem system nixpkgs-nixos.lib.platforms.linux;

  # mkNixosSystem is a convenience function for declaring a nixos system,
  # and integrating it with home-manager.
  mkNixosSystem =
    { hostname
    , username ? "belak"
    , system ? "x86_64-linux"
    , nixosModules ? [ ]
    , homeModules ? [ ]
    }:
    nixpkgs-nixos.lib.nixosSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs-nixos;

      modules = [
        ./modules/nixos/common.nix

        home-manager.nixosModules.home-manager

        # Configure home-manager and set up a user so home-manager can pick up
        # the homeDirectory.
        {
          home-manager.useGlobalPkgs = true;
          users.users.${username}.home = "/home/${username}";
          home-manager.users.${username}.imports = [
            ./modules/home/common.nix
            ./modules/home/linux.nix
          ] ++ homeModules;
        }
      ] ++ (optionalPath ./hosts/nixos/${hostname}) ++ nixosModules;
    };

  # mkDarwinSystem is a convenience function for declaring a nix-darwin system,
  # and integrating it with home-manager.
  mkDarwinSystem =
    { hostname
    , username ? "belak"
    , system ? "aarch64-darwin"
    , darwinModules ? [ ]
    , homeModules ? [ ]
    }:
    darwin.lib.darwinSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs-darwin;

      modules = [
        ./modules/darwin/common.nix

        home-manager.darwinModules.home-manager

        # Configure home-manager and set up a user so home-manager can pick up
        # the homeDirectory.
        {
          home-manager.useGlobalPkgs = true;
          users.users.${username}.home = "/Users/${username}";
          home-manager.users.${username}.imports = [
            ./modules/home/common.nix
            ./modules/home/darwin.nix
          ] ++ homeModules;
        }
      ] ++ (optionalPath ./hosts/darwin/${hostname}) ++ darwinModules;
    };

  # mkHome is a convenience function for declaring a home-manager setup with our
  # specific package setup.
  mkHome =
    { username ? "belak"
    , system ? "x86_64-linux"
    , homeModules ? [ ]
    }: home-manager.lib.homeManagerConfiguration {
      pkgs = mkPkgs system
        (if isDarwin system
        then nixpkgs-darwin
        else nixpkgs-nixos);

      modules = [
        ./modules/home/common.nix
      ] ++ homeModules;
    };
}
