{
  description = "Belak's Polyglot Nix configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

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
      agenix,
      nixpkgs-unstable,
      ...
    }:
    let
      lib = import ./nix/lib.nix inputs;
    in
    {
      inherit lib;

      overlays = import ./nix/overlays.nix inputs;

      darwinModules.default = import ./nix/darwin/modules;
      homeModules.default = import ./nix/home/modules;
      nixosModules.default = import ./nix/nixos/modules;

      # There are a number of different formatters available: nixfmt, alejandra,
      # and nixfmt-rfc-style. As rfc-style is the "up-and-coming" format, we use
      # that rather than stock nixfmt.
      formatter = lib.forAllSystems (
        system:
        let
          pkgs = nixpkgs-unstable.legacyPackages.${system};
        in
        pkgs.treefmt.withConfig {
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
        }
      );

      devShells = lib.forAllSystems (
        system:
        let
          pkgs = nixpkgs-unstable.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              agenix.packages.${system}.agenix
              deploy-rs
              nixos-anywhere
            ];
          };
        }
      );

      darwinConfigurations = {
        "baku" = lib.mkDarwinSystem {
          modules = [ ./nix/darwin/hosts/baku.nix ];
        };

        "melinoe" = lib.mkDarwinSystem {
          modules = [
            ./nix/darwin/hosts/melinoe.nix
          ];
        };
      };

      nixosConfigurations = {
        # Thinkpad T14 (i5)
        "beatrix" = lib.mkNixosSystem {
          modules = [
            ./nix/nixos/hosts/beatrix
            ./nix/nixos/users/belak
          ];
        };

        # ThinkCentre M93p
        "eiko" = lib.mkNixosSystem {
          modules = [
            ./nix/nixos/hosts/eiko
            ./nix/nixos/users/belak
          ];
        };

        # Beelink Mini S12 Pro
        "freya" = lib.mkNixosSystem {
          modules = [
            ./nix/nixos/hosts/freya
            ./nix/nixos/users/belak
          ];
        };

        # MacBook UTM Instance
        "hades" = lib.mkNixosSystem {
          modules = [
            ./nix/nixos/hosts/hades
            ./nix/nixos/users/belak
          ];
        };

        # ThinkPad X13s Gen 1
        "quina" = lib.mkNixosSystem {
          modules = [
            ./nix/nixos/hosts/quina
            ./nix/nixos/users/belak
          ];
        };

        # ThinkPad T460
        "zidane" = lib.mkNixosSystem {
          modules = [
            ./nix/nixos/hosts/zidane
            ./nix/nixos/users/belak
          ];
        };

        # Primary Desktop
        "zorn" = lib.mkNixosSystem {
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
        "belak@baku" = lib.mkHome {
          system = "x86_64-darwin";
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/baku.nix
          ];
        };
        "belak@beatrix" = lib.mkHome {
          system = "x86_64-linux";
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/beatrix.nix
          ];
        };
        "belak@eiko" = lib.mkHome {
          system = "x86_64-linux";
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/eiko.nix
          ];
        };
        "belak@freya" = lib.mkHome {
          system = "x86_64-linux";
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/freya.nix
          ];
        };
        "belak@hades" = lib.mkHome {
          system = "aarch64-linux";
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/hades.nix
          ];
        };
        "belak@quina" = lib.mkHome {
          system = "aarch64-linux";
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/quina.nix
          ];
        };
        "belak@melinoe" = lib.mkHome {
          system = "aarch64-darwin";
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/melinoe.nix
          ];
        };
        "belak@zidane" = lib.mkHome {
          system = "x86_64-linux";
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/zidane.nix
          ];
        };
        "belak@zorn" = lib.mkHome {
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
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations."belak@eiko";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.eiko;
        };

        freya = {
          hostname = "freya.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations."belak@freya";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.freya;
        };

        zidane = {
          hostname = "zidane.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations."belak@zidane";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.zidane;
        };
      };

      packages = lib.forAllSystems (system: {
        install-iso-minimal = import ./nix/pkgs/install-iso-minimal.nix inputs;
        rpi-bootstrap-image = import ./nix/pkgs/rpi-bootstrap-image.nix inputs;
        x13s-install-iso-minimal = import ./nix/pkgs/rpi-bootstrap-image.nix inputs;
      });
    };
}
