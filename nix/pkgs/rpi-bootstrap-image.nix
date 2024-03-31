{ nixos-generators, ... }:
# The default Raspberry Pi image doesn't provide a way to SSH in by
# default, so we build a custom image to make bootstrapping easier.
#
# Note that eventually it should be possible to set up a
# nixosConfiguration for each device and build a pre-configured image,
# but that's a "for later" problem.
nixos-generators.nixosGenerate {
  system = "aarch64-linux";
  format = "sd-image-belak-rpi";

  modules = [
    (
      { pkgs, ... }:
      {
        sdImage.compressImage = false;

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
}
