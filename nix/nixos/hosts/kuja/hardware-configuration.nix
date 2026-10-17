{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "nvme"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/8e88d2f0-edc5-4f55-a78e-0a60d457c43a";
    fsType = "ext4";
  };

  swapDevices = [ ];

  # Slightly cursed brcm-firmware from the t2-linux project, which extracts it
  # from a macOS image. It takes a bit of space, but it's the best way to track
  # it without needing to have a bunch of random firmware files just copied
  # around.
  hardware.apple-t2.firmware.enable = true;

  nixpkgs.allowedUnfree = [
    "brcm-firmware"
    "brcm-firmware-sonoma"
    "brcm-firmware-sonoma-zstd"
  ];

  networking.networkmanager = {
    enable = true;

    # There's an internal adapter which shows up as a USB adapter -
    # NetworkManager sends errors relating to this interface regularly because
    # it thinks it's connected, but can't get an address, so we just tell
    # NetworkManager to pretend it doesn't exist.
    unmanaged = [ "interface-name:enp230s0f1u1" ];
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  hardware.enableRedistributableFirmware = lib.mkDefault true;
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
