# Build a secret-free UEFI snapshot for provisioning Vultr instances.
# Build and provisioning notes: README.md.
{ modulesPath, pkgs, ... }:
{
  imports = [
    ../../profiles/vultr.nix
    (modulesPath + "/virtualisation/disk-image.nix")
  ];

  image = {
    baseName = "nixos-vultr";
    format = "raw";
    efiSupport = true;
  };

  networking = {
    hostName = "vultr-bootstrap";
    useDHCP = true;
  };

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
    openssh.authorizedKeys.keys = (import ../../../../secrets/keys.nix).users;
  };

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  environment.systemPackages = [ pkgs.git ];
  time.timeZone = "Etc/UTC";
  system.stateVersion = "26.05";
}
