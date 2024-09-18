{
  self,
  nixpkgs-nixos,
  nixpkgs-darwin,
  nixos-cosmic,
  nixos-hardware,
  agenix,
  home-manager,
  darwin,
  ...
}:
rec {
  # We could use nixpkgs-nixos.lib.systems.flakeExposed, but I prefer to only
  # expose architectures I actually run.
  forAllSystems = nixpkgs-nixos.lib.genAttrs [
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
      system ? "x86_64-linux",
      nixpkgs ? nixpkgs-nixos,
    }:
    nixpkgs.lib.nixosSystem {
      inherit system;

      pkgs = import nixpkgs {
        inherit system;
      };

      modules = [
        self.nixosModules.default
        agenix.nixosModules.default
        nixos-cosmic.nixosModules.default
      ] ++ modules;

      # Pass extra inputs through to all modules.
      specialArgs = {
        inherit self;
        inherit nixos-hardware;
      };
    };

  # mkDarwinSystem is a convenience function for declaring a nix-darwin system.
  mkDarwinSystem =
    {
      system ? "aarch64-darwin",
      nixpkgs ? nixpkgs-darwin,
      modules ? [ ],
    }:
    darwin.lib.darwinSystem {
      inherit system;

      specialArgs = {
        inherit self;
      };

      pkgs = import nixpkgs {
        inherit system;
      };

      modules = [
        self.darwinModules.default
        agenix.darwinModules.default
      ] ++ modules;
    };

  # mkHome is a convenience function for declaring a home-manager config with
  # our specific package setup.
  mkHome =
    {
      system ? "x86_64-linux",
      username ? "belak",
      nixpkgs ? nixpkgs-nixos,
      modules ? [ ],
    }:
    home-manager.lib.homeManagerConfiguration {
      extraSpecialArgs = {
        inherit self;
      };

      pkgs = import nixpkgs {
        inherit system;
      };

      modules = [
        self.homeModules.default
        agenix.homeManagerModules.default
      ] ++ modules;
    };

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
