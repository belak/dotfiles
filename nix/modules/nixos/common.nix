{ pkgs, ... }: {
  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';

    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
      warn-dirty = false;
    };

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 2d";
    };
  };

  environment.systemPackages = with pkgs; [
    home-manager
  ];
}
