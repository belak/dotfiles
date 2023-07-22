{ config, lib, pkgs, nixos-hardware, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    nixos-hardware.nixosModules.lenovo-thinkpad-t14
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  #boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # Hardware quirks

  # For some reason the touchpad buttons on my laptop don't work by default.
  # This param tells the driver to use a secondary bus for the device which
  # seems to fix the issue.
  boot.kernelParams = [ "psmouse.synaptics_intertouch=0" ];

  # We need to specify our video driver because it clears the console font when
  # loaded. This works around the race condition by making sure the video driver
  # is loaded as early as possible during the boot process.
  boot.initrd.kernelModules = [ "i915" ];

  # Filesystems

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/10459668-a6bf-4180-a17a-0851d15a326e";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/CA01-2E05";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/5fc3dc0b-2a22-4daf-86ad-16792138c6fb"; }];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp0s31f6.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp0s20f3.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
