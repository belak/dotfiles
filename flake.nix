{
  description = "Belak's Polyglot Nix configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

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

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.darwin.follows = "darwin";
      inputs.home-manager.follows = "home-manager";
    };

    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    belak-blog = {
      url = "github:belak/blog";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };

    belak-btta = {
      url = "github:belak/btta";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
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
          # SFF desktop (i5-8500T, 16GB)
          "garnet" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/garnet
              ./nix/nixos/users/belak
            ];
            homeUsers = {
              belak = ./nix/home/users/belak/garnet.nix;
            };
          };

          # Beelink Mini S12 Pro
          "freya" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/freya
              ./nix/nixos/users/belak
            ];
            homeUsers = {
              belak = ./nix/home/users/belak/freya.nix;
            };
          };

          # ThinkPad X13s Gen 1
          "quina" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/quina
              ./nix/nixos/users/belak
            ];
            homeUsers = {
              belak = ./nix/home/users/belak/quina.nix;
            };
          };

          # Intel NUC (i7-8650U, 32GB)
          "vivi" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/vivi
              ./nix/nixos/users/belak
            ];
            homeUsers = {
              belak = ./nix/home/users/belak/vivi.nix;
            };
          };

          # ThinkPad T460
          "zidane" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/zidane
              ./nix/nixos/users/belak
            ];
            homeUsers = {
              belak = ./nix/home/users/belak/zidane.nix;
            };
          };
        };

        # There are some things nixos and nix-darwin can't provide; for everything
        # else there's home-manager.
        #
        # Note that this is no longer used - it is left around in case it's
        # needed in the future.
        homeConfigurations = { };

        deploy.nodes = {
          garnet = {
            hostname = "garnet.elwert.dev";
            profilesOrder = [ "system" ];
            profiles.system = myLib.mkNixosDeploy self.nixosConfigurations.garnet;
          };

          freya = {
            hostname = "freya.elwert.dev";
            profilesOrder = [ "system" ];
            profiles.system = myLib.mkNixosDeploy self.nixosConfigurations.freya;
          };

          quina = {
            hostname = "quina.elwert.dev";
            profilesOrder = [ "system" ];
            profiles.system = myLib.mkNixosDeploy self.nixosConfigurations.quina;
          };

          vivi = {
            hostname = "vivi.elwert.dev";
            profilesOrder = [ "system" ];
            profiles.system = myLib.mkNixosDeploy self.nixosConfigurations.vivi;
          };

          zidane = {
            hostname = "zidane.elwert.dev";
            profilesOrder = [ "system" ];
            profiles.system = myLib.mkNixosDeploy self.nixosConfigurations.zidane;
          };
        };
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
          # Normally we'd use the default nixpkgs here, but unfortunately we
          # want to prefer deploy-rs from our overlay to avoid long install
          # times from source.
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = builtins.attrValues self.overlays;
            config = { };
          };

          # There are a number of different formatters available: nixfmt, alejandra,
          # and nixfmt-rfc-style. As rfc-style is the "up-and-coming" format, we use
          # that rather than stock nixfmt.
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
              packages = with pkgs; [
                agenix
                attic-client
                deploy-rs
                nixos-anywhere
              ];
            };
          };

          packages = lib.packagesFromDirectoryRecursive {
            inherit (pkgs) callPackage;
            directory = ./nix/pkgs;
          };
        };
    };
}
