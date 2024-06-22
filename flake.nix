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
          modules = [
            ./nix/hosts/nixos/kupo
            ./nix/users/nixos/belak
          ];
        };
        "stiltzkin" = lib.mkNixosSystem {
          hostname = "stiltzkin";
          system = "aarch64-linux";
          modules = [
            ./nix/hosts/nixos/stiltzkin
            ./nix/users/nixos/belak
          ];
        };
        "moguo" = lib.mkNixosSystem {
          hostname = "moguo";
          system = "aarch64-linux";
          modules = [
            ./nix/hosts/nixos/moguo
            ./nix/users/nixos/belak
          ];
        };
        "monty" = lib.mkNixosSystem {
          hostname = "monty";
          system = "aarch64-linux";
          modules = [
            ./nix/hosts/nixos/monty
            ./nix/users/nixos/belak
          ];
        };

        # ThinkCentre M93p
        "eiko" = lib.mkNixosSystem {
          hostname = "eiko";
          modules = [
            ./nix/hosts/nixos/eiko
            ./nix/users/nixos/belak
          ];
        };

        # Intel NUC7i7DNHE
        "vivi" = lib.mkNixosSystem {
          hostname = "vivi";
          modules = [
            ./nix/hosts/nixos/vivi
            ./nix/users/nixos/belak
          ];
        };

        # Primary Laptop (ThinkPad T14 Gen 1)
        "zagreus" = lib.mkNixosSystem {
          hostname = "zagreus";
          modules = [
            ./nix/hosts/nixos/zagreus
            ./nix/users/nixos/belak
          ];
        };

        # Old Laptop (ThinkPad T460)
        "zidane" = lib.mkNixosSystem {
          hostname = "zidane";
          modules = [
            ./nix/hosts/nixos/zidane
            ./nix/users/nixos/belak
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
        "belak" = lib.mkHome { };
        "belak@zagreus" = lib.mkHome { hostname = "zagreus"; };
      };

      deploy.nodes = {
        kupo = {
          hostname = "kupo.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.kupo;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
        };

        stiltzkin = {
          hostname = "stiltzkin.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.stiltzkin;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
        };

        moguo = {
          hostname = "moguo.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.moguo;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
        };

        monty = {
          hostname = "monty.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.monty;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
        };

        eiko = {
          hostname = "eiko.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.eiko;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
        };

        vivi = {
          hostname = "vivi.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.vivi;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
        };

        zidane = {
          hostname = "zidane.elwert.dev";
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.zidane;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
        };
      };

      packages = lib.forAllSystems (system: {
        rpi-bootstrap-image = import ./nix/pkgs/rpi-bootstrap-image.nix inputs;
        install-iso-minimal = import ./nix/pkgs/install-iso-minimal.nix inputs;
      });
    };
}
