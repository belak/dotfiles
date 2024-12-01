{
  description = "Belak's Polyglot Nix configuration";

  inputs = {
    nixpkgs-nixos.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-24.11-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
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

    # Note that even though logically we should make nixos-cosmic.inputs.nixpkgs
    # "follow" nixpkgs-unstable, we need to use the inputs provided by that
    # flake in order for the provided binary cache to work.
    nixos-cosmic = {
      url = "github:lilyinstarlight/nixos-cosmic";
      #inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
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
      formatter = lib.forAllSystems (system: nixpkgs-unstable.legacyPackages.${system}.nixfmt-rfc-style);

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
        # Raspberry Pis
        "artemicion" = lib.mkNixosSystem {
          system = "aarch64-linux";
          modules = [
            ./nix/nixos/hosts/artemicion
            ./nix/nixos/users/belak
          ];
        };
        "kupo" = lib.mkNixosSystem {
          system = "aarch64-linux";
          modules = [
            ./nix/nixos/hosts/kupo
            ./nix/nixos/users/belak
          ];
        };
        "stiltzkin" = lib.mkNixosSystem {
          system = "aarch64-linux";
          modules = [
            ./nix/nixos/hosts/stiltzkin
            ./nix/nixos/users/belak
          ];
        };
        "moguo" = lib.mkNixosSystem {
          system = "aarch64-linux";
          modules = [
            ./nix/nixos/hosts/moguo
            ./nix/nixos/users/belak
          ];
        };
        "monty" = lib.mkNixosSystem {
          system = "aarch64-linux";
          modules = [
            ./nix/nixos/hosts/monty
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

        # Intel NUC7i7DNHE
        "vivi" = lib.mkNixosSystem {
          modules = [
            ./nix/nixos/hosts/vivi
            ./nix/nixos/users/belak
          ];
        };

        # Primary Laptop (ThinkPad T14 Gen 1)
        "zagreus" = lib.mkNixosSystem {
          modules = [
            ./nix/nixos/hosts/zagreus
            ./nix/nixos/users/belak
          ];
        };

        # Old Laptop (ThinkPad T460)
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
        "belak" = lib.mkHome { modules = [ ./nix/home/users/belak ]; };
        "belak-arm64" = lib.mkHome {
          system = "aarch64-linux";
          modules = [ ./nix/home/users/belak ];
        };
        "belak@baku" = lib.mkHome {
          system = "x86_64-darwin";
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/baku.nix
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
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/zagreus.nix
          ];
        };
        "belak@zorn" = lib.mkHome {
          modules = [
            ./nix/home/users/belak
            ./nix/home/users/belak/zorn.nix
          ];
        };
      };

      deploy.nodes = {
        artemicion = {
          hostname = "artemicion.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.artemicion;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak-arm64;
        };

        kupo = {
          hostname = "kupo.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.kupo;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak-arm64;
        };

        stiltzkin = {
          hostname = "stiltzkin.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.stiltzkin;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak-arm64;
        };

        moguo = {
          hostname = "moguo.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.moguo;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak-arm64;
        };

        monty = {
          hostname = "monty.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.monty;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak-arm64;
        };

        eiko = {
          hostname = "eiko.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.eiko;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
        };

        vivi = {
          hostname = "vivi.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
          profiles.system = lib.mkNixosDeploy self.nixosConfigurations.vivi;
          profiles.belak = lib.mkHomeDeploy self.homeConfigurations.belak;
        };

        zidane = {
          hostname = "zidane.elwert.dev";
          profilesOrder = [
            "belak"
            "system"
          ];
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
