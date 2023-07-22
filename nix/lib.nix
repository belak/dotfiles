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
  forAllSystems = nixpkgs-unstable.lib.genAttrs nixpkgs-unstable.lib.systems.flakeExposed;

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

  optionalFile = path: if builtins.pathExists path then [ path ] else [ ];

  isDarwin = system: builtins.elem system nixpkgs-darwin.lib.platforms.darwin;

  isLinux = system: builtins.elem system nixpkgs-nixos.lib.platforms.linux;

  # mkNixosSystem is a convenience function for declaring a nixos system,
  # and integrating it with home-manager.
  mkNixosSystem =
    { hostname
    , username ? "belak"
    , system ? "x86_64-linux"
    , extraNixosModules ? [ ]
    }:
    nixpkgs-nixos.lib.nixosSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs-nixos;

      modules = baseNixosModules ++
        (optionalFile ./hosts/nixos/${hostname}) ++
        [
          {
            users.users.${username}.home = "/home/${username}";
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
    }:
    darwin.lib.darwinSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs-darwin;

      modules = baseDarwinModules ++
        (optionalFile ./hosts/darwin/${hostname}) ++
        [
          {
            users.users.${username}.home = "/Users/${username}";
          }
        ] ++ extraDarwinModules;
    };

  # mkHome is a convenience function for declaring a home-manager setup with our
  # specific package setup.
  mkHome =
    { username ? "belak"
    , system ? "x86_64-linux"
    , nixpkgs ? nixpkgs-nixos
    , hostname ? null
    , extraHomeModules ? [ ]
    }:
    home-manager.lib.homeManagerConfiguration {
      pkgs = mkPkgs system nixpkgs;

      modules = baseHomeModules ++ [
        {
          home = {
            username = username;
            homeDirectory = (
              if isDarwin system
              then "/Users/${username}"
              else "/home/${username}"
            );
          };
        }
      ] ++ (mkOptionals (hostname != null) (optionalFile ./hosts/home/${hostname}.nix)) ++
        extraHomeModules;
    };
}
