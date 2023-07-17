{ self
, nixpkgs-nixos
, nixpkgs-darwin
, nixpkgs-unstable
, nixos-hardware
, home-manager
, darwin
, ...
} @ inputs:

let
  baseNixosModules = builtins.attrValues (import ./modules/nixos);
  baseDarwinModules = builtins.attrValues (import ./modules/darwin);
  baseHomeModules = builtins.attrValues (import ./modules/home);
in
rec {
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

  mkOptionals = check: data: if check then data else [ ];

  isDarwin = system: builtins.elem system nixpkgs-darwin.lib.platforms.darwin;

  isLinux = system: builtins.elem system nixpkgs-nixos.lib.platforms.linux;

  # mkNixosSystem is a convenience function for declaring a nixos system,
  # and integrating it with home-manager.
  mkNixosSystem =
    { hostname
    , username ? "belak"
    , system ? "x86_64-linux"
    , extraNixosModules ? [ ]
    , extraHomeModules ? [ ]
    }:
    nixpkgs-nixos.lib.nixosSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs-nixos;

      modules = baseNixosModules ++ [
        ./hosts/nixos/${hostname}

        home-manager.nixosModules.home-manager

        # Configure home-manager and set up a user so home-manager can pick up
        # the homeDirectory.
        {
          home-manager.useGlobalPkgs = true;
          users.users.${username}.home = "/home/${username}";
          home-manager.users.${username}.imports = baseHomeModules ++ [
            ./hosts/home/${hostname}.nix
          ] ++ extraHomeModules;
        }
      ] ++ extraNixosModules;

      specialArgs = {
        inherit nixos-hardware;
      };
    };

  # mkDarwinSystem is a convenience function for declaring a nix-darwin system,
  # and integrating it with home-manager.
  mkDarwinSystem =
    { hostname
    , username ? "belak"
    , system ? "aarch64-darwin"
    , extraDarwinModules ? [ ]
    , extraHomeModules ? [ ]
    }:
    darwin.lib.darwinSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs-darwin;

      modules = baseDarwinModules ++ [
        ./hosts/darwin/${hostname}

        home-manager.darwinModules.home-manager

        # Configure home-manager and set up a user so home-manager can pick up
        # the homeDirectory.
        {
          home-manager.useGlobalPkgs = true;
          users.users.${username}.home = "/Users/${username}";
          home-manager.users.${username}.imports = baseHomeModules ++ [
            ./hosts/home/${hostname}.nix
          ] ++ extraHomeModules;
        }
      ] ++ extraDarwinModules;
    };

  # mkHome is a convenience function for declaring a home-manager setup with our
  # specific package setup.
  mkHome =
    { username ? "belak"
    , system ? "x86_64-linux"
    , hostname ? null
    , extraHomeModules ? [ ]
    }: home-manager.lib.homeManagerConfiguration {
      pkgs = mkPkgs system
        (if isDarwin system
        then nixpkgs-darwin
        else nixpkgs-nixos);

      modules = baseHomeModules ++
        (mkOptionals (hostname != null) [ ./hosts/home/${hostname}.nix ]) ++
        extraHomeModules;
    };
}
