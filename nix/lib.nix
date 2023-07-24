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

  mkPkgs = pkgs: pkgs // {
    # It's easiest to configure our unfree packages for every nixpkgs input
    # rather than on a system-by-system basis.
    config.allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
      "discord"
      "hplip"
      "obsidian"
      "skypeforlinux"
    ];

    overlays = builtins.attrValues self.overlays;
  };

  systemHome = system: username: if isDarwin system then "/Users/${username}" else "/home/${username}";

  mkOptionals = check: data: if check then data else [ ];

  optionalFile = path: if builtins.pathExists path then [ path ] else [ ];

  isDarwin = system: builtins.elem system nixpkgs-darwin.lib.platforms.darwin;

  isLinux = system: builtins.elem system nixpkgs-nixos.lib.platforms.linux;

  # mkNixosSystem is a convenience function for declaring a nixos system,
  # and integrating it with home-manager.
  mkNixosSystem =
    { hostname
    , pkgs ? nixpkgs-nixos.legacyPackages.x86_64-linux
    , username ? "belak"
    , extraNixosModules ? [ ]
    }:
    nixpkgs-nixos.lib.nixosSystem {
      inherit (pkgs) system;

      pkgs = mkPkgs pkgs;

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
    , pkgs ? nixpkgs-darwin.legacyPackages.aarch64-darwin
    , extraDarwinModules ? [ ]
    }:
    darwin.lib.darwinSystem {
      inherit (pkgs) system;

      pkgs = mkPkgs pkgs;

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
    { pkgs ? nixpkgs-nixos.legacyPackages.x86_64-linux
    , username ? "belak"
    , hostname ? null
    , extraHomeModules ? [ ]
    }:
    home-manager.lib.homeManagerConfiguration {
      pkgs = mkPkgs pkgs;

      modules = baseHomeModules ++
        (mkOptionals (hostname != null) (optionalFile ./hosts/home/${hostname}.nix)) ++
        [
          {
            belak = {
              username = username;
              homeDirectory = systemHome pkgs.system username;
            };
          }
        ] ++ extraHomeModules;
    };
}
