{
  description = "Belak's Polyglot Nix configuration";

  inputs = {
    nixpkgs-nixos.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-23.05-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
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

      formatter = lib.forAllSystems
        (system: nixpkgs-unstable.legacyPackages.${system}.nixpkgs-fmt);

      nixosConfigurations = {
        "auron" = lib.mkNixosSystem {
          hostname = "auron";
        };

        "zagreus" = lib.mkNixosSystem {
          hostname = "zagreus";
        };
      };

      # There are some things nixos and nix-darwin can't provide; for everything
      # else there's home-manager.
      homeConfigurations = {
        "belak@auron" = lib.mkHome {
          hostname = "auron";
        };

        "belak@zagreus" = lib.mkHome {
          hostname = "zagreus";
        };

        "belak" = lib.mkHome { };
      };
    };
}
