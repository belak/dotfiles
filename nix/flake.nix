{
  description = "Home Manager configuration of belak";

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

  outputs = inputs @ { nixpkgs-nixos, nixpkgs-darwin, nixos-hardware, ... }:
    let
      lib = import ./lib inputs;
    in
    {
      formatter = {
        x86_64-linux = nixpkgs-nixos.legacyPackages.x86_64-linux.nixpkgs-fmt;
        aarch64-darwin = nixpkgs-darwin.legacyPackages.aarch64-darwin.nixpkgs-fmt;
        x86_64-darwin = nixpkgs-darwin.legacyPackages.x86_64-darwin.nixpkgs-fmt;
      };

      nixosConfigurations."zagreus" = lib.mkNixosSystem {
        hostname = "zagreus";
        extraModules = [
          nixos-hardware.nixosModules.lenovo-thinkpad-t14
        ];
      };

      darwinConfigurations."COMP-JY4T0D6C0V" = lib.mkDarwinSystem {
        hostname = "COMP-JY4T0D6C0V";
        username = "kaleb.elwert";
      };

      # There are some things nixos and nix-darwin can't provide; for everything
      # else there's home-manager.
      homeConfigurations."belak" = lib.mkHome { };
      homeConfigurations."kaleb.elwert" = lib.mkHome {
        system = "aarch64-darwin";
      };
    };
}
