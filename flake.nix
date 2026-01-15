{
  description = "Belak's Polyglot Nix configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
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
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.11";
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

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-x13s = {
      url = "github:BrainWart/x13s-nixos";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
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
          "baku" = myLib.mkDarwinSystem {
            modules = [ ./nix/darwin/hosts/baku.nix ];
          };

          "melinoe" = myLib.mkDarwinSystem {
            modules = [
              ./nix/darwin/hosts/melinoe.nix
            ];
          };
        };

        nixosConfigurations = {
          # Thinkpad T14 (i5)
          "beatrix" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/beatrix
              ./nix/nixos/users/belak
            ];
          };

          # ThinkCentre M93p
          "eiko" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/eiko
              ./nix/nixos/users/belak
            ];
          };

          # Beelink Mini S12 Pro
          "freya" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/freya
              ./nix/nixos/users/belak
            ];
          };

          # MacBook UTM Instance
          "hades" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/hades
              ./nix/nixos/users/belak
            ];
          };

          # ThinkPad X13s Gen 1
          "quina" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/quina
              ./nix/nixos/users/belak
            ];
          };

          # ThinkPad T460
          "zidane" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/zidane
              ./nix/nixos/users/belak
            ];
          };

          # Primary Desktop
          "zorn" = myLib.mkNixosSystem {
            modules = [
              ./nix/nixos/hosts/zorn
              ./nix/nixos/users/belak
            ];
          };
        };

        # There are some things nixos and nix-darwin can't provide; for everything
        # else there's home-manager.
        #
        # It's better for these to be host-specific configs, so we can still have
        # host-specific configuration, but we provide a default "belak" fallback
        # for the most common cases.
        homeConfigurations = {
          "belak@baku" = myLib.mkHome {
            system = "x86_64-darwin";
            modules = [
              ./nix/home/users/belak
              ./nix/home/users/belak/baku.nix
            ];
          };
          "belak@beatrix" = myLib.mkHome {
            system = "x86_64-linux";
            modules = [
              ./nix/home/users/belak
              ./nix/home/users/belak/beatrix.nix
            ];
          };
          "belak@eiko" = myLib.mkHome {
            system = "x86_64-linux";
            modules = [
              ./nix/home/users/belak
              ./nix/home/users/belak/eiko.nix
            ];
          };
          "belak@freya" = myLib.mkHome {
            system = "x86_64-linux";
            modules = [
              ./nix/home/users/belak
              ./nix/home/users/belak/freya.nix
            ];
          };
          "belak@hades" = myLib.mkHome {
            system = "aarch64-linux";
            modules = [
              ./nix/home/users/belak
              ./nix/home/users/belak/hades.nix
            ];
          };
          "belak@quina" = myLib.mkHome {
            system = "aarch64-linux";
            modules = [
              ./nix/home/users/belak
              ./nix/home/users/belak/quina.nix
            ];
          };
          "belak@melinoe" = myLib.mkHome {
            system = "aarch64-darwin";
            modules = [
              ./nix/home/users/belak
              ./nix/home/users/belak/melinoe.nix
            ];
          };
          "belak@zidane" = myLib.mkHome {
            system = "x86_64-linux";
            modules = [
              ./nix/home/users/belak
              ./nix/home/users/belak/zidane.nix
            ];
          };
          "belak@zorn" = myLib.mkHome {
            system = "x86_64-linux";
            modules = [
              ./nix/home/users/belak
              ./nix/home/users/belak/zorn.nix
            ];
          };
        };

        deploy.nodes = {
          eiko = {
            hostname = "eiko.elwert.dev";
            profilesOrder = [
              "belak"
              "system"
            ];
            profiles.belak = myLib.mkHomeDeploy self.homeConfigurations."belak@eiko";
            profiles.system = myLib.mkNixosDeploy self.nixosConfigurations.eiko;
          };

          freya = {
            hostname = "freya.elwert.dev";
            profilesOrder = [
              "belak"
              "system"
            ];
            profiles.belak = myLib.mkHomeDeploy self.homeConfigurations."belak@freya";
            profiles.system = myLib.mkNixosDeploy self.nixosConfigurations.freya;
          };

          zidane = {
            hostname = "zidane.elwert.dev";
            profilesOrder = [
              "belak"
              "system"
            ];
            profiles.belak = myLib.mkHomeDeploy self.homeConfigurations."belak@zidane";
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
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = builtins.attrValues self.overlays;
            config = { };
          };

          # There are a number of different formatters available: nixfmt, alejandra,
          # and nixfmt-rfc-style. As rfc-style is the "up-and-coming" format, we use
          # that rather than stock nixfmt.
          formatter = pkgs.treefmt.withConfig {
            runtimeInputs = [ pkgs.nixfmt-rfc-style ];

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
                inputs.agenix.packages.${system}.agenix
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
