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

  outputs = inputs @ { nixpkgs-unstable, ... }:
    let
      lib = import ./lib.nix inputs;
      overlays = import ./overlays.nix inputs;
    in
    {
      inherit lib;
      inherit overlays;

      formatter = {
        x86_64-linux = nixpkgs-unstable.legacyPackages.x86_64-linux.nixpkgs-fmt;
        aarch64-darwin = nixpkgs-unstable.legacyPackages.aarch64-darwin.nixpkgs-fmt;
        x86_64-darwin = nixpkgs-unstable.legacyPackages.x86_64-darwin.nixpkgs-fmt;
      };

      nixosConfigurations."auron" = lib.mkNixosSystem {
        hostname = "auron";
      };

      nixosConfigurations."zagreus" = lib.mkNixosSystem {
        hostname = "zagreus";
      };

      # There are some things nixos and nix-darwin can't provide; for everything
      # else there's home-manager.
      homeConfigurations."belak" = lib.mkHome { };
    };
}
