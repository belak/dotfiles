# Build a minimal UEFI disk image for bootstrapping Vultr instances.
{ modulesPath, pkgs, ... }:
{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    (modulesPath + "/virtualisation/disk-image.nix")
  ];

  nixpkgs.hostPlatform = "x86_64-linux";
  system.stateVersion = "26.05";

  image = {
    baseName = "nixos-vultr";
    format = "raw";
    efiSupport = true;
  };

  boot.loader.efi.canTouchEfiVariables = false;
  boot.kernelParams = [
    "console=tty0"
    "console=ttyS0,115200n8"
  ];

  networking = {
    hostName = "vultr-bootstrap";
    useDHCP = true;
  };

  services.qemuGuest.enable = true;
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };

  users.mutableUsers = false;
  users.users.root = {
    hashedPassword = "!";
    openssh.authorizedKeys.keys = (import ../../../secrets/keys.nix).users;
  };

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  environment.systemPackages = [ pkgs.git ];
  time.timeZone = "Etc/UTC";
}
