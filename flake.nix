# Configures personal systems, packages, and development shells.
{
  description = "Belak's Polyglot Nix configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    nixos-hardware-t2-fork = {
      url = "github:soopyc/nixos-hardware/apple-t2-updates";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      agenix,
      ...
    }:
    let
      myLib = import ./nix/lib.nix inputs;
    in

    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;

      flake = {
        lib = myLib;

        darwinModules.default = import ./nix/darwin/modules;
        homeModules.default = import ./nix/home/modules;
        nixosModules.default = import ./nix/nixos/modules;

        overlays = import ./nix/overlays.nix inputs;

        darwinConfigurations = {
          "melinoe" = myLib.mkDarwinSystem {
            modules = [
              ./nix/darwin/hosts/melinoe.nix
              ./nix/darwin/users/belak
            ];
            homeUsers = {
              belak = ./nix/home/users/belak/melinoe.nix;
            };
          };
        };

        nixosConfigurations = {
          # Intel MacBook Pro (2020)
          "kuja" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/kuja
              ./nix/nixos/users/belak
            ];
            homeUsers = {
              belak = ./nix/home/users/belak/kuja.nix;
            };
          };
        };

        # There are some things nixos and nix-darwin can't provide; for everything
        # else there's home-manager.
        #
        # Note that this is no longer used - it is left around in case it's
        # needed in the future.
        homeConfigurations = { };
      };

      perSystem =
        {
          pkgs,
          system,
          config,
          lib,
          ...
        }:
        {
          # Normally we'd use the default nixpkgs here, but we want to install
          # agenix.
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              agenix.overlays.default
            ];
            config = { };
          };

          formatter = pkgs.treefmt.withConfig {
            runtimeInputs = [ pkgs.nixfmt ];

            settings = {
              # Log level for files treefmt won't format
              on-unmatched = "info";

              # Configure nixfmt for .nix files
              formatter.nixfmt = {
                command = "nixfmt";
                includes = [ "*.nix" ];
              };
            };
          };

          devShells = {
            default = pkgs.mkShell {
              packages = [
                pkgs.agenix
              ];
            };
          };
        };
    };
}
