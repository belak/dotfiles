{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "zagreus";
    domain = "elwert.dev";
    networkmanager.enable = true;
  };

  time.timeZone = "US/Pacific";

  i18n.defaultLocale = "en_US.UTF-8";

  console = with pkgs; {
    earlySetup = true;
    font = "${terminus_font}/share/consolefonts/ter-114n.psf.gz";
    packages = [ terminus_font ];
  };

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
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
  };

  # We keep global packages pretty minimal - essentially only what we'd need to
  # set up the rest of the system.
  environment.systemPackages = with pkgs; [
    git
    gutenprint
    hplipWithPlugin
    vim
  ];

  environment.gnome.excludePackages = (with pkgs; [
    gnome-photos
    gnome-tour
  ]) ++ (with pkgs.gnome; [
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

  programs.zsh = {
    enable = true;

    # The default setup does a bunch of weird things so we disable all of them.
    # This lets us properly set them up in our user-level zshrc.
    promptInit = "";
    setOptions = [ ];
    enableGlobalCompInit = false;
  };

  # List services that you want to enable:

  services.fwupd.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
