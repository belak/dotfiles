{ pkgs, ... }: {
  environment.systemPackages = [
    pkgs.vim
  ];

  homebrew = {
    enable = true;

    brews = [ ];

    masApps = {
      Parcel = 639968404;
      Reeder = 1529448980;
      Things = 904280696;
    };
  };

  nix.settings = {
    auto-optimise-store = true;
    build-users-group = "nixbld";
    experimental-features = "nix-command flakes";
    trusted-users = [ "root" "kaleb.elwert" ];
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
