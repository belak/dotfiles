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

  mkOptionals = check: data: if check then data else [ ];

  optionalFile = path: if builtins.pathExists path then [ path ] else [ ];

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
        [
          self.nixosModules.default
          home-manager.nixosModules.home-manager
          {
            # Use the nixos pkgs we just configured rather than a separate
            # variable.
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ]
        ++ (optionalFile ./hosts/nixos/${hostname})
        ++ builtins.attrValues (
          builtins.mapAttrs (username: extraHomeModules: {
            users.users.${username}.home = "/home/${username}";
            home-manager.users.${username} = {
              # Using the modules as "imports" should be pretty much the same
              # thing as "modules" in a homeConfiguration.
              imports = mkHomeModules { inherit hostname username extraHomeModules; };
            };
          }) configuredUsers
        )
        ++ extraNixosModules;

      # Pass extra inputs through to all modules.
      specialArgs = {
        inherit nixos-hardware;
      };
    };

  mkColmenaNode = node: {
    deployment = {
      targetHost = "${node}.elwert.dev";
      targetUser = "root";
    };

    nixpkgs.system = self.nixosConfigurations.${node}.config.nixpkgs.system;
    imports = self.nixosConfigurations.${node}._module.args.modules;
  };

  # mkDarwinSystem is a convenience function for declaring a nix-darwin system,
  # and integrating it with home-manager.
  mkDarwinSystem =
    {
      nixpkgs ? nixpkgs-darwin,
      system ? "aarch64-darwin",
      configuredUsers ? {
        "belak" = [ ];
      },
      hostname ? null,
      extraDarwinModules ? [ ],
    }:
    darwin.lib.darwinSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs;

      modules =
        [
          self.darwinModules.default
          home-manager.darwinModules.home-manager
          {
            # Use the nixos pkgs we just configured rather than a separate
            # variable.
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ]
        ++ (mkOptionals (hostname != null) (optionalFile ./hosts/darwin/${hostname}.nix))
        ++ builtins.attrValues (
          builtins.mapAttrs (username: extraHomeModules: {
            users.users.${username}.home = "/Users/${username}";
            home-manager.users.${username} = {
              # Using the modules as "imports" should be pretty much the same
              # thing as "modules" in a homeConfiguration.
              imports = mkHomeModules { inherit hostname username extraHomeModules; };
            };
          }) configuredUsers
        )
        ++ extraDarwinModules;
    };

  # mkHomeModules is used by mkNixosSystem, mkDarwinSystem and mkHome to allow
  # all to use the same modules. This allows us to have our config in our system
  # configurations and still use the same setup if we're using home-manager
  # standalone.
  mkHomeModules =
    {
      hostname,
      username,
      extraHomeModules,
    }:
    [ self.homeModules.default ]
    ++ (mkOptionals (hostname != null) (optionalFile ./users/${username}/${hostname}.nix))
    ++ (optionalFile ./users/${username}/default.nix)
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

      modules = mkHomeModules { inherit hostname username extraHomeModules; } ++ [
        {
          # Let Home Manager install and manage itself. Note that we set this
          # up *only* when calling mkHome because other setups should use
          # home-manager via their nix-darwin and nixos modules.
          programs.home-manager.enable = true;
        }
      ];
    };
}
