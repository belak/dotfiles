{
  self,
  nixpkgs,
  nixos-x13s,
  agenix,
  disko,
  home-manager,
  darwin,
  ...
}:
{
  # We could use nixpkgs.lib.systems.flakeExposed, but I prefer to only
  # expose architectures I actually run.
  forAllSystems = nixpkgs.lib.genAttrs [
    "aarch64-linux"
    "x86_64-linux"
    "aarch64-darwin"
    "x86_64-darwin"
  ];

  mkOptionals = check: data: if check then data else [ ];

  optionalFile = path: if builtins.pathExists path then [ path ] else [ ];

  # mkNixosSystem is a convenience function for declaring a nixos system.
  mkNixosSystem =
    {
      modules,
      homeUsers ? {},
    }:
    nixpkgs.lib.nixosSystem {
      specialArgs = {
        inherit self;
        inherit agenix;
      };

      modules = [
        self.nixosModules.default
        agenix.nixosModules.default
        disko.nixosModules.disko
        home-manager.nixosModules.default
        nixos-x13s.nixosModules.default
        { home-manager.users = homeUsers; }
      ]
      ++ modules;
    };

  # mkDarwinSystem is a convenience function for declaring a nix-darwin system.
  mkDarwinSystem =
    {
      modules,
      homeUsers ? {},
    }:
    darwin.lib.darwinSystem {
      # We sometimes turn this on when testing against the nix-darwin master
      # branch. This is used to bypass a confidence check which makes sure
      # you're using a stable nixpkgs with stable nix-darwin.
      #
      #enableNixpkgsReleaseCheck = false;

      specialArgs = {
        inherit self;
        inherit agenix;
      };

      modules = [
        self.darwinModules.default
        agenix.darwinModules.default
        home-manager.darwinModules.default
        { home-manager.users = homeUsers; }
      ]
      ++ modules;
    };

  # mkHome is a convenience function for declaring a home-manager config.
  mkHome =
    {
      modules,
      system,
    }:
    home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};

      extraSpecialArgs = {
        inherit self;
      };

      modules = [
        self.homeModules.default
        agenix.homeManagerModules.default
      ]
      ++ modules;
    };

  # mkNixosDeploy takes a nixosConfig, generated using mkNixosSystem, and
  # generates an opinionated deploy-rs config.
  mkNixosDeploy =
    nixosConfig:
    let
      pkgs = nixosConfig._module.args.pkgs;
    in
    {
      user = "root";
      sshUser = "root";
      path = pkgs.deploy-rs.lib.activate.nixos nixosConfig;
    };

  # mkHomeDeploy takes a homeManagerConfig, generated using mkHome, and
  # generates an opinionated deploy-rs config.
  mkHomeDeploy =
    homeManagerConfig:
    let
      pkgs = homeManagerConfig.pkgs;
    in
    {
      user = homeManagerConfig.config.home.username;
      sshUser = "root";
      path = pkgs.deploy-rs.lib.activate.home-manager homeManagerConfig;
    };
}
