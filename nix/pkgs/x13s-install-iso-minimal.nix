{ nixos-generators, ... }:
# The Thinkpad x13s is supported by mainline Linux, but for some reason it
# doesn't seem to properly load the relevant dtb and device drivers. We also
# include a number of kernel params to work around hardware quirks.
nixos-generators.nixosGenerate {
  system = "aarch64-linux";
  format = "installation-cd-minimal";

  modules = [
    (
      { pkgs, ... }:
      {
        hardware.deviceTree.filter = "sc8280xp-*.dtb";

        boot.kernelParams = [
          "clk_ignore_unused"
          "pd_ignore_unused"
          "arm64.nopauth"
          "cma=128M"
        ];

        initrd.availableKernelModules = [
          "i2c-core"
          "i2c-hid"
          "i2c-hid-of"
          "i2c-qcom-geni"
          "pcie-qcom"
          "phy-qcom-qmp-combo"
          "phy-qcom-qmp-pcie"
          "phy-qcom-qmp-usb"
          "phy-qcom-snps-femto-v2"
          "phy-qcom-usb-hs"
        ];
      }
    )
  ];
}
