{
  description = "Belak's Polyglot Nix configuration";

  inputs = {
    nixpkgs-nixos.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-23.05-darwin";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };
  };

  outputs = inputs @ { self, nixpkgs-unstable, nixos-hardware, ... }:
    let
      lib = import ./lib.nix inputs;
      overlays = import ./overlays.nix inputs;
      homeModules = import ./modules/home inputs;
      darwinModules = import ./modules/darwin inputs;
      nixosModules = import ./modules/nixos inputs;
    in
    {
      inherit lib;
      inherit overlays;
      inherit homeModules;
      inherit darwinModules;
      inherit nixosModules;

      formatter = {
        x86_64-linux = nixpkgs-unstable.legacyPackages.x86_64-linux.nixpkgs-fmt;
        aarch64-darwin = nixpkgs-unstable.legacyPackages.aarch64-darwin.nixpkgs-fmt;
        x86_64-darwin = nixpkgs-unstable.legacyPackages.x86_64-darwin.nixpkgs-fmt;
      };

      nixosConfigurations."zagreus" = self.lib.mkNixosSystem {
        hostname = "zagreus";
        nixosModules = with nixosModules; [
          nixos-hardware.nixosModules.lenovo-thinkpad-t14
          common
        ];
        homeModules = with homeModules; [
          cli
          dev
          dotfiles
          gnome
          gui
        ];
      };

      # There are some things nixos and nix-darwin can't provide; for everything
      # else there's home-manager.
      homeConfigurations."belak" = self.lib.mkHome {
        homeModules = with homeModules; [
          dotfiles
        ];
      };
    };
}
