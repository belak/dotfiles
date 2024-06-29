{
  self,
  nixpkgs-nixos,
  nixpkgs-darwin,
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
          "1password-cli"
          "android-studio-stable"
          "discord"
          "hplip"
          "obsidian"
          "rar"
          "skypeforlinux"
        ];

      config.permittedInsecurePackages = [ ];
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
      nixpkgs ? nixpkgs-nixos,
    }:
    nixpkgs-nixos.lib.nixosSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs (builtins.attrValues self.overlays);

      modules = [
        self.nixosModules.default
        agenix.nixosModules.default
      ] ++ modules;

      # Pass extra inputs through to all modules.
      specialArgs = {
        inherit nixos-hardware;
      };
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

  # mkDarwinSystem is a convenience function for declaring a nix-darwin system,
  # and integrating it with home-manager.
  mkDarwinSystem =
    {
      system ? "aarch64-darwin",
      hostname ? null,
      nixpkgs ? nixpkgs-darwin,
      modules ? [ ],
    }:
    darwin.lib.darwinSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs (builtins.attrValues self.overlays);

      modules = [
        self.darwinModules.default
        agenix.darwinModules.default
      ] ++ modules;
    };

  # mkHomeModules is used by mkNixosSystem, mkDarwinSystem and mkHome to allow
  # all to use the same modules. This allows us to have our config in our system
  # configurations and still use the same setup if we're using home-manager
  # standalone.
  mkHomeModules =
    {
      hostname,
      username,
      modules,
    }:
    [
      self.homeModules.default
      agenix.homeManagerModules.default
    ]
    ++ (mkOptionals (hostname != null) (optionalFile ./users/home/${username}/${hostname}.nix))
    ++ (optionalFile ./users/home/${username}/default.nix)
    ++ modules;

  # mkHome is a convenience function for declaring a home-manager setup with our
  # specific package setup.
  mkHome =
    {
      system ? "x86_64-linux",
      username ? "belak",
      hostname ? null,
      nixpkgs ? nixpkgs-nixos,
      modules ? [ ],
    }:
    home-manager.lib.homeManagerConfiguration {
      pkgs = mkPkgs system nixpkgs (builtins.attrValues self.overlays);

      modules = mkHomeModules { inherit hostname username modules; } ++ [
        {
          # Let Home Manager install and manage itself.
          programs.home-manager.enable = true;
        }
      ];
    };
}
