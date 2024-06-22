{
  description = "Belak's Polyglot Nix configuration";

  inputs = {
    nixpkgs-nixos.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-24.05-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
    };

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs-nixos,
      nixpkgs-unstable,
      nixos-generators,
      nixos-hardware,
      deploy-rs,
      ...
    }:
    let
      lib = import ./nix/lib.nix inputs;
    in
    {
      inherit lib;

      overlays = import ./nix/overlays.nix inputs;
      darwinModules.default = import ./nix/modules/darwin;
      homeModules.default = import ./nix/modules/home;
      nixosModules.default = import ./nix/modules/nixos;

      # There are a number of different formatters available: nixfmt, alejandra,
      # and nixfmt-rfc-style. As rfc-style is the "up-and-coming" format, we use
      # that rather than stock nixfmt.
      formatter = lib.forAllSystems (system: nixpkgs-unstable.legacyPackages.${system}.nixfmt-rfc-style);

      nixosConfigurations = {
        # Raspberry Pis
        "kupo" = lib.mkNixosSystem {
          hostname = "kupo";
          system = "aarch64-linux";
          modules = [ ./nix/hosts/nixos/kupo ];
        };
        "stiltzkin" = lib.mkNixosSystem {
          hostname = "stiltzkin";
          system = "aarch64-linux";
          modules = [ ./nix/hosts/nixos/stiltzkin ];
        };
        "moguo" = lib.mkNixosSystem {
          hostname = "moguo";
          system = "aarch64-linux";
          modules = [ ./nix/hosts/nixos/moguo ];
        };
        "monty" = lib.mkNixosSystem {
          hostname = "monty";
          system = "aarch64-linux";
          modules = [ ./nix/hosts/nixos/monty ];
        };

        # ThinkCentre M93p
        "eiko" = lib.mkNixosSystem {
          hostname = "eiko";
          modules = [ ./nix/hosts/nixos/eiko ];
        };

        # Intel NUC7i7DNHE
        "vivi" = lib.mkNixosSystem {
          hostname = "vivi";
          modules = [ ./nix/hosts/nixos/vivi ];
        };

        # Primary Laptop (ThinkPad T14 Gen 1)
        "zagreus" = lib.mkNixosSystem {
          hostname = "zagreus";
          modules = [ ./nix/hosts/nixos/zagreus ];
        };

        # Old Laptop (ThinkPad T460)
        "zidane" = lib.mkNixosSystem {
          hostname = "zidane";
          modules = [ ./nix/hosts/nixos/zidane ];
        };
      };

      # There are some things nixos and nix-darwin can't provide; for everything
      # else there's home-manager.
      #
      # Most of our home-manager config either uses the nixos or nix-darwin
      # modules. When that isn't possible, we can use home-manager standalone.
      #
      # It's better for these to be host-specific configs, so we can still have
      # host-specific configuration, but we provide a default "belak" fallback
      # for the most common cases.
      homeConfigurations = {
        "belak" = lib.mkHome { };
      };

      deploy.nodes = {
        kupo = {
          hostname = "kupo.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.kupo;
        };

        stiltzkin = {
          hostname = "stiltzkin.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.stiltzkin;
        };

        moguo = {
          hostname = "moguo.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.moguo;
        };

        monty = {
          hostname = "monty.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.monty;
        };

        eiko = {
          hostname = "eiko.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.eiko;
        };

        vivi = {
          hostname = "vivi.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.vivi;
        };

        zidane = {
          hostname = "zidane.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.zidane;
        };
      };

      packages = lib.forAllSystems (system: {
        rpi-bootstrap-image = import ./nix/pkgs/rpi-bootstrap-image.nix inputs;
        install-iso-minimal = import ./nix/pkgs/install-iso-minimal.nix inputs;
      });
    };
}
