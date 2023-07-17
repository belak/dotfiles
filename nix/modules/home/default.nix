{
  common = import ./common.nix;

  # Platforms
  darwin = import ./darwin.nix;
  linux = import ./linux.nix;

  # Feature modules
  dev = import ./dev.nix;
  dotfiles = import ./dotfiles.nix;
  gnome = import ./gnome.nix;
  gui = ./gui.nix;
}
