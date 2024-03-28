{ pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  networking = {
    hostName = "zagreus";
    domain = "elwert.dev";
    networkmanager.enable = true;

  };

  belak.dev.enable = true;
  belak.dev.armEmulation = true;
  belak.laptop.enable = true;

  # Enable fprintd for fingerprint auth. Note that this is disabled for now
  # because the gdm behavior disallows password auth.
  #
  #services.fprintd.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.excludePackages = [ pkgs.xterm ];

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # We use tlp to automatically configure some power saving settings. This
  # requires us to disable power-profiles-daemon explicitly which comes by
  # default as a part of Gnome 40.
  services.tlp = {
    enable = true;
    settings = {
      # Note that because we're on a modern intel CPU, the only CPU governors we
      # have are "performance" and "powersave".
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
    };
  };
  services.power-profiles-daemon.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  users.users.belak = {
    isNormalUser = true;
    description = "Kaleb Elwert";
    extraGroups = [
      "networkmanager"
      "wheel"
      "dialout"
    ];
    subUidRanges = [
      {
        startUid = 100000;
        count = 65536;
      }
    ];
    subGidRanges = [
      {
        startGid = 100000;
        count = 65536;
      }
    ];
    shell = pkgs.zsh;
  };

  environment.gnome.excludePackages =
    (with pkgs; [
      gnome-photos
      gnome-tour
    ])
    ++ (with pkgs.gnome; [
      cheese # webcam tool
      gnome-maps # map tool
      gnome-music # music player
      gedit # text editor
      epiphany # web browser
      geary # email reader
      gnome-characters # font/character viewer
      simple-scan # scanner utility
      totem # video player
    ]);

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
