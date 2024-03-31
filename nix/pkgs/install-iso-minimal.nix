{ nixos-generators, ... }:
nixos-generators.nixosGenerate {
  system = "x86_64-linux";
  format = "install-iso-minimal";

  customFormats = {
    install-iso-minimal =
      { modulesPath, lib, ... }:
      {
        imports = [ "${toString modulesPath}/installer/cd-dvd/installation-cd-minimal.nix" ];

        security.sudo.enable = true;

        users.users.belak = {
          isNormalUser = true;
          description = "Kaleb Elwert";
          initialPassword = "hunter2";
          extraGroups = [ "wheel" ];
        };

        # Enable ssh at boot
        services.openssh.enable = true;

        formatAttr = "isoImage";
        fileExtension = ".iso";
      };
  };
}
