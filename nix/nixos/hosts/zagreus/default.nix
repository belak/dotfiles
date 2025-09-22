{ pkgs, ... }:
{
  imports = [
    ./disko-config.nix

    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "zagreus";
    domain = "elwert.dev";
    networkmanager = {
      enable = true;
      plugins = with pkgs; [ networkmanager-l2tp ];
      wifi.backend = "iwd";
    };
  };

  belak = {
    dev.enable = true;
    #dev.armEmulation = true;
    #gnome.enable = true;
    laptop.enable = true;
  };

  #virtualisation.libvirtd.enable = true;
  #programs.virt-manager.enable = true;
  #users.users.belak.extraGroups = [ "libvirtd" ];

  environment.systemPackages = with pkgs; [
    pciutils
    pptp

    intel-media-driver
    vaapiIntel
  ];

  services.nats = {
    enable = true;
    jetstream = true;
  };

  # TODO: at the moment this is enabled purely so the hostKeys are available to
  # agenix. Ideally that wouldn't be necessary.
  services.openssh.enable = true;

  # Enable fprintd for fingerprint auth. Note that this is disabled for now
  # because the gdm behavior disallows password auth.
  #
  #services.fprintd.enable = true;

  # Enable the X11 windowing system.
  #services.xserver.enable = true;
  #services.xserver.excludePackages = [ pkgs.xterm ];

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # List services that you want to enable:

  services.postgresql = {
    enable = true;
    ensureDatabases = [ "seabird" ];
    ensureUsers = [
      {
        name = "belak";
        ensureClauses.superuser = true;
      }
    ];
    authentication = ''
      #type database  DBuser  auth-method
      local all       all     trust
    '';
  };

  services.fwupd.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
