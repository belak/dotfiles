{
  description = "Belak's Polyglot Nix configuration";

  inputs = {
    nixpkgs-nixos.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-24.11-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.nixpkgs-stable.follows = "nixpkgs-nixos";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
    };

    darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-24.11";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
      inputs.darwin.follows = "darwin";
      inputs.home-manager.follows = "home-manager";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
    };

    nixos-x13s = {
      url = "github:BrainWart/x13s-nixos";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs =
    inputs@{
      self,
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

      darwinConfigurations = {
        "baku" = lib.mkDarwinSystem {
          system = "x86_64-darwin";
          modules = [ ];
        };

        "melinoe" = lib.mkDarwinSystem {
          system = "aarch64-darwin";
          modules = [
            ./nix/darwin/hosts/melinoe.nix
          ];
        };
      };

      nixosConfigurations = {
        # ThinkCentre M93p
        "eiko" = lib.mkNixosSystem {
          system = "x86_64-linux";
          modules = [
            ./nix/nixos/hosts/eiko
            ./nix/nixos/users/belak
          ];
        };

        # Beelink Mini S12 Pro
        "freya" = lib.mkNixosSystem {
          system = "x86_64-linux";
          modules = [
            ./nix/nixos/hosts/freya
            ./nix/nixos/users/belak
          ];
        };

        # ThinkPad X13s Gen 1
        "quina" = lib.mkNixosSystem {
          system = "aarch64-linux";
          modules = [
            ./nix/nixos/hosts/quina
            ./nix/nixos/users/belak
          ];
        };

        # ThinkPad T14 Gen 1
        "zagreus" = lib.mkNixosSystem {
          system = "x86_64-linux";
          modules = [
            ./nix/nixos/hosts/zagreus
            ./nix/nixos/users/belak
          ];
        };

        # ThinkPad T460
        "zidane" = lib.mkNixosSystem {
          system = "x86_64-linux";
          modules = [
            ./nix/nixos/hosts/zidane
            ./nix/nixos/users/belak
          ];
        };

        # Primary Desktop
        "zorn" = lib.mkNixosSystem {
          system = "x86_64-linux";
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
        "belak" = lib.mkHome {
          system = "x86_64-linux";
          modules = [ ./nix/home/users/belak ];
        };
        "belak@baku" = lib.mkHome {
          system = "x86_64-darwin";
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/baku.nix
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
        "belak@zagreus" = lib.mkHome {
          system = "x86_64-linux";
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/zagreus.nix
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
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.eiko;
        };

        freya = {
          hostname = "freya.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.freya;
        };

        vivi = {
          hostname = "vivi.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.vivi;
        };

        zidane = {
          hostname = "zidane.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
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
