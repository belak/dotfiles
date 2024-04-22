{
  self,
  nixpkgs-nixos,
  nixos-hardware,
  home-manager,
  darwin,
  ...
}@inputs:
let
  pkgsCache = self.lib.forAllSystems (
    system: self.lib.mkPkgs system nixpkgs-nixos (builtins.attrValues self.overlays)
  );
in
rec {
  # We could use nixpkgs-nixos.lib.systems.flakeExposed, but I prefer to only
  # expose architectures I actually run.
  forAllSystems = nixpkgs-nixos.lib.genAttrs [
    "aarch64-linux"
    "x86_64-linux"
    "aarch64-darwin"
    #"x86_64-darwin"
  ];

  mkPkgs =
    system: nixpkgs: overlays:
    import nixpkgs {
      inherit system;
      inherit overlays;

      config.allowUnfreePredicate =
        pkg:
        builtins.elem (nixpkgs.lib.getName pkg) [
          "android-studio-stable"
          "discord"
          "hplip"
          "obsidian"
          "rar"
          "skypeforlinux"
        ];

      config.permittedInsecurePackages = [ "nix-2.15.3" ];
    };

  mkOptionals = check: data: if check then data else [ ];

  optionalFile = path: if builtins.pathExists path then [ path ] else [ ];

  # mkNixosSystem is a convenience function for declaring a nixos system,
  # and integrating it with home-manager.
  mkNixosSystem =
    {
      hostname,
      modules,
      system ? "x86_64-linux",
      configuredUsers ? {
        "belak" = [ ];
      },
    }:
    nixpkgs-nixos.lib.nixosSystem {
      inherit system;

      pkgs = pkgsCache.${system};

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
        ++ modules;

      # Pass extra inputs through to all modules.
      specialArgs = {
        inherit nixos-hardware;
      };
    };

  # mkDarwinSystem is a convenience function for declaring a nix-darwin system,
  # and integrating it with home-manager.
  mkDarwinSystem =
    {
      system ? "aarch64-darwin",
      configuredUsers ? {
        "belak" = [ ];
      },
      hostname ? null,
      extraDarwinModules ? [ ],
    }:
    darwin.lib.darwinSystem {
      inherit system;

      pkgs = pkgsCache.${system};

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
      system ? "x86_64-linux",
      username ? "belak",
      hostname ? null,
      extraHomeModules ? [ ],
    }:
    home-manager.lib.homeManagerConfiguration {
      pkgs = pkgsCache.${system};

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
