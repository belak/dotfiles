{
  config,
  lib,
  pkgs,
  nixos-hardware-t2-fork,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    # This is arguably hardware-related
    nixos-hardware-t2-fork.nixosModules.apple-t2
  ];

  networking = {
    hostName = "kuja";
    domain = "elwert.dev";
  };

  # Enable touchBar support. Due to a hardware quirk, this shuts down on
  # suspend but never starts up on resume, so we have to manually configure
  # that.
  hardware.apple.touchBar.enable = true;
  systemd.services.tiny-dfr = {
    wantedBy = [ "suspend.target" ];
    after = [ "suspend.target" ];
  };

  belak = {
    gnome.enable = true;
    gui.enable = true;
  };

  programs.firefox.enable = true;

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  environment.systemPackages = with pkgs; [
    git
    htop
    ghostty
    neovim
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "26.05"; # Did you read the comment?
}
