{
  self,
  nixpkgs-nixos,
  nixpkgs-darwin,
  nixpkgs-unstable,
  nixos-hardware,
  home-manager,
  darwin,
  ...
} @ inputs: let
  baseNixosModules = builtins.attrValues (import ./modules/nixos);
  baseDarwinModules = builtins.attrValues (import ./modules/darwin);
  baseHomeModules = builtins.attrValues (import ./modules/home);
in rec {
  forAllSystems = nixpkgs-unstable.lib.genAttrs nixpkgs-unstable.lib.systems.flakeExposed;

  mkPkgs = system: nixpkgs:
    import nixpkgs {
      inherit system;

      # It's easiest to configure our unfree packages for every nixpkgs input
      # rather than on a system-by-system basis.
      config.allowUnfreePredicate = pkg:
        builtins.elem (nixpkgs.lib.getName pkg) [
          "android-studio-stable"
          "discord"
          "hplip"
          "obsidian"
          "rar"
          "skypeforlinux"
        ];

      config.permittedInsecurePackages = [
        "electron-25.9.0"
      ];

      overlays = builtins.attrValues self.overlays;
    };

  systemHome = system: username:
    if isDarwin system
    then "/Users/${username}"
    else "/home/${username}";

  mkOptionals = check: data:
    if check
    then data
    else [];

  optionalFile = path:
    if builtins.pathExists path
    then [path]
    else [];

  isDarwin = system: builtins.elem system nixpkgs-darwin.lib.platforms.darwin;

  isLinux = system: builtins.elem system nixpkgs-nixos.lib.platforms.linux;

  # mkNixosSystem is a convenience function for declaring a nixos system,
  # and integrating it with home-manager.
  mkNixosSystem = {
    hostname,
    nixpkgs ? nixpkgs-nixos,
    system ? "x86_64-linux",
    username ? "belak",
    extraNixosModules ? [],
  }:
    nixpkgs.lib.nixosSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs;

      modules =
        baseNixosModules
        ++ (optionalFile ./hosts/nixos/${hostname})
        ++ [
          {
            users.users.${username}.home = "/home/${username}";
          }
        ]
        ++ extraNixosModules;

      specialArgs = {
        inherit nixos-hardware;
      };
    };

  # mkDarwinSystem is a convenience function for declaring a nix-darwin system,
  # and integrating it with home-manager.
  mkDarwinSystem = {
    nixpkgs ? nixpkgs-darwin,
    system ? "aarch64-darwin",
    username ? "belak",
    hostname ? null,
    extraDarwinModules ? [],
  }:
    darwin.lib.darwinSystem {
      inherit system;

      pkgs = mkPkgs system nixpkgs;

      modules =
        baseDarwinModules
        ++ (mkOptionals (hostname != null) (optionalFile ./hosts/darwin/${hostname}.nix))
        ++ [
          {
            users.users.${username}.home = "/Users/${username}";
          }
        ]
        ++ extraDarwinModules;
    };

  # mkHome is a convenience function for declaring a home-manager setup with our
  # specific package setup.
  mkHome = {
    nixpkgs ? nixpkgs-nixos,
    system ? "x86_64-linux",
    username ? "belak",
    hostname ? null,
    extraHomeModules ? [],
  }:
    home-manager.lib.homeManagerConfiguration {
      pkgs = mkPkgs system nixpkgs;

      modules =
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
    };
}
