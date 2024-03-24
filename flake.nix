{
  description = "Belak's Polyglot Nix configuration";

  inputs = {
    nixpkgs-nixos.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-23.11-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs-nixos";
    };
  };

  outputs =
    inputs@{ nixpkgs-unstable, nixos-generators, ... }:
    let
      lib = import ./nix/lib.nix inputs;
      overlays = import ./nix/overlays.nix inputs;
    in
    {
      inherit lib;
      inherit overlays;

      # There are a number of different formatters available: nixfmt, alejandra,
      # and nixfmt-rfc-style. As rfc-style is the "up-and-coming" format, we use
      # that rather than stock nixfmt.
      formatter = lib.forAllSystems (system: nixpkgs-unstable.legacyPackages.${system}.nixfmt-rfc-style);

      nixosConfigurations = {
        "zagreus" = lib.mkNixosSystem { hostname = "zagreus"; };
        "zidane" = lib.mkNixosSystem { hostname = "zidane"; };
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

      packages = lib.forAllSystems (
        system: {
          # The default Raspberry Pi image doesn't provide a way to SSH in by
          # default, so we build a custom image to make bootstrapping easier.
          #
          # Note that eventually it should be possible to set up a
          # nixosConfiguration for each device and build a pre-configured image,
          # but that's a "for later" problem.
          rpi-bootstrap-image = nixos-generators.nixosGenerate {
            system = "aarch64-linux";
            format = "sd-image-belak-rpi";

            modules = [
              (
                { pkgs, ... }:
                {
                  security.sudo.enable = true;

                  users.users.belak = {
                    isNormalUser = true;
                    description = "Kaleb Elwert";
                    initialPassword = "hunter2";
                    extraGroups = [ "wheel" ];
                  };

                  services.openssh.enable = true;
                }
              )
            ];

            customFormats = {
              sd-image-belak-rpi =
                { modulesPath, ... }:
                {
                  imports = [ "${toString modulesPath}/installer/sd-card/sd-image-aarch64.nix" ];

                  formatAttr = "sdImage";
                };
            };
          };

          install-iso-minimal = nixos-generators.nixosGenerate {
            system = system;
            format = "install-iso-minimal";

            customFormats = {
              install-iso-minimal =
                { modulesPath, lib, ... }:
                {
                  imports = [ "${toString modulesPath}/installer/cd-dvd/installation-cd-minimal.nix" ];

                  # Enable ssh at boot
                  services.openssh.enable = true;

                  formatAttr = "isoImage";
                  fileExtension = ".iso";
                };
            };
          };
        }
      );
    };
}
