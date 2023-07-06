{
  description = "Home Manager configuration of belak";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";

    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-23.05-darwin";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, nixpkgs-darwin, nixpkgs-unstable, darwin, home-manager, ... }: {
    formatter = {
      x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
      aarch64-darwin = nixpkgs-darwin.legacyPackages.aarch64-darwin.nixpkgs-fmt;
    };

    nixosConfigurations."zagreus" = nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      modules = [
        ./nixos/configuration.nix
      ];
    };

    darwinConfigurations."COMP-JY4T0D6C0V" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      pkgs = nixpkgs-darwin.legacyPackages.aarch64-darwin;

      modules = [ ./nix-darwin/default.nix ];
    };

    # My work computers tend to use the username kaleb.elwert, and are macbooks,
    # while I tend to use "belak" for all my personal machines. This provides a
    # convenient way to differentiate them.

    homeConfigurations."belak" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;

      extraSpecialArgs = {
        pkgs-unstable = nixpkgs-unstable.legacyPackages.x86_64-linux;
      };

      modules = [
        ./modules/dotfiles.nix
        ./home-manager/linux.nix
      ];
    };

    homeConfigurations."kaleb.elwert" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs-darwin.legacyPackages.aarch64-darwin;

      modules = [
        ./modules/dotfiles.nix
        ./home-manager/darwin.nix
      ];
    };
  };
}
