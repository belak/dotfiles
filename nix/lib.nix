{
  self,
  nixpkgs-nixos,
  nixpkgs-darwin,
  nixpkgs-unstable,
  nixos-hardware,
  home-manager,
  darwin,
  ...
}@inputs:
let
  baseNixosModules = builtins.attrValues (import ./modules/nixos);
  baseDarwinModules = builtins.attrValues (import ./modules/darwin);
  baseHomeModules = builtins.attrValues (import ./modules/home);
in
rec {
  forAllSystems = nixpkgs-unstable.lib.genAttrs nixpkgs-unstable.lib.systems.flakeExposed;

  mkPkgs =
    system: nixpkgs:
    import nixpkgs {
      inherit system;

      # It's easiest to configure our unfree packages for every nixpkgs input
      # rather than on a system-by-system or module-by-module basis.
      #
      # Note that confusingly unstable packages are configured in the unstable
      # overlay, not here.
      config.allowUnfreePredicate =
        pkg:
        builtins.elem (nixpkgs.lib.getName pkg) [
          "android-studio-stable"
          "discord"
          "hplip"
          "rar"
          "skypeforlinux"
        ];

      config.permittedInsecurePackages = [ ];

      overlays = builtins.attrValues self.overlays;
    };

  systemHome =
    system: username: if isDarwin system then "/Users/${username}" else "/home/${username}";

  mkOptionals = check: data: if check then data else [ ];

  optionalFile = path: if builtins.pathExists path then [ path ] else [ ];

  isDarwin = system: builtins.elem system nixpkgs-darwin.lib.platforms.darwin;

  isLinux = system: builtins.elem system nixpkgs-nixos.lib.platforms.linux;

  # mkNixosSystem is a convenience function for declaring a nixos system,
  # and integrating it with home-manager.
  mkNixosSystem =
    {
      hostname,
      nixpkgs ? nixpkgs-nixos,
      system ? "x86_64-linux",
      configuredUsers ? {
        "belak" = [ ];
      },
      extraNixosModules ? [ ],
    }:
    nixpkgs.lib.nixosSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs;

      modules =
        baseNixosModules
        ++ [
          home-manager.nixosModules.home-manager
          {
            # Use the nixos pkgs we just configured
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ]
        ++ (optionalFile ./hosts/nixos/${hostname})
        ++ builtins.attrValues (
          builtins.mapAttrs
            (username: extraHomeModules: {
              users.users.${username}.home = "/home/${username}";
              home-manager.users.${username} = {
                imports = mkHomeModules {
                  inherit
                    system
                    hostname
                    username
                    extraHomeModules
                    ;
                };
              };
            })
            configuredUsers
        )
        ++ extraNixosModules;

      # Pass extra inputs through to all modules.
      specialArgs = {
        inherit nixos-hardware;
      };
    };

  # mkDarwinSystem is a convenience function for declaring a nix-darwin system,
  # and integrating it with home-manager.
  mkDarwinSystem =
    {
      nixpkgs ? nixpkgs-darwin,
      system ? "aarch64-darwin",
      username ? "belak",
      hostname ? null,
      extraDarwinModules ? [ ],
    }:
    darwin.lib.darwinSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs;

      modules =
        baseDarwinModules
        ++ (mkOptionals (hostname != null) (optionalFile ./hosts/darwin/${hostname}.nix))
        ++ [ { users.users.${username}.home = "/Users/${username}"; } ]
        ++ extraDarwinModules;
    };

  # mkHomeModules is used by both mkNixosSystem and mkHome to allow both to use
  # the same modules. This allows us to have our config in our
  # nixosConfiguration and still use the same setup if we're using home-manager
  # standalone.
  mkHomeModules =
    {
      system,
      hostname,
      username,
      extraHomeModules,
    }:
    baseHomeModules
    ++ (mkOptionals (hostname != null) (optionalFile ./hosts/home/${hostname}.nix))
    ++ [
      {
        belak = {
          username = username;
          homeDirectory = systemHome system username;
        };
      }
    ]
    ++ extraHomeModules;

  # mkHome is a convenience function for declaring a home-manager setup with our
  # specific package setup.
  mkHome =
    {
      nixpkgs ? nixpkgs-nixos,
      system ? "x86_64-linux",
      username ? "belak",
      hostname ? null,
      extraHomeModules ? [ ],
    }:
    home-manager.lib.homeManagerConfiguration {
      pkgs = mkPkgs system nixpkgs;

      modules = mkHomeModules {
        inherit
          system
          hostname
          username
          extraHomeModules
          ;
      };
    };
}
