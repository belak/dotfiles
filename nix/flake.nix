{
  description = "Belak's Polyglot Nix configuration";

  inputs = {
    nixpkgs-nixos.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
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

  outputs = inputs @ { self, nixpkgs-nixos, nixpkgs-darwin, nixos-hardware, ... }:
    let
      lib = import ./lib.nix inputs;
      overlays = import ./overlays.nix inputs;
      hmModules = import ./modules inputs;
    in
    {
      inherit lib;
      inherit overlays;
      inherit hmModules;

      formatter = {
        x86_64-linux = nixpkgs-nixos.legacyPackages.x86_64-linux.nixpkgs-fmt;
        aarch64-darwin = nixpkgs-darwin.legacyPackages.aarch64-darwin.nixpkgs-fmt;
        x86_64-darwin = nixpkgs-darwin.legacyPackages.x86_64-darwin.nixpkgs-fmt;
      };

      nixosConfigurations."zagreus" = self.lib.mkNixosSystem {
        hostname = "zagreus";
        nixosModules = [
          nixos-hardware.nixosModules.lenovo-thinkpad-t14
        ];
        hmModules = [
          ./modules/linux.nix
          ./modules/dotfiles.nix
          ./modules/go.nix
          ./modules/rust.nix
        ];
      };

      darwinConfigurations."COMP-JY4T0D6C0V" = self.lib.mkDarwinSystem {
        hostname = "COMP-JY4T0D6C0V";
        username = "kaleb.elwert";
        hmModules = [
          ./modules/darwin.nix
          ./modules/dotfiles.nix
          ./modules/go.nix
          ./modules/rust.nix
        ];
      };

      # There are some things nixos and nix-darwin can't provide; for everything
      # else there's home-manager.
      homeConfigurations."belak" = self.lib.mkHome {
        hmModules = [
          ./modules/dotfiles.nix
        ];
      };
    };
}
