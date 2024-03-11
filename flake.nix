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
  };

  outputs =
    inputs@{ nixpkgs-unstable, ... }:
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
    };
}
