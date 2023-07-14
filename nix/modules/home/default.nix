inputs: {
  # NOTE: We specifically don't call these imports - this is needed so they can
  # be imported in flakes which depend on this one and used as home-manager
  # modules. They may rely on additional setup from self.lib.mkDarwinSystem or
  # self.lib.mkHome

  common = import ./common.nix;

  # Platforms
  darwin = import ./darwin.nix;
  linux = import ./linux.nix;

  # Feature modules
  cli = import ./cli.nix;
  dev = import ./dev.nix;
  dotfiles = import ./dotfiles.nix;
  gnome = import ./gnome.nix;
  gui = import ./gui.nix;
}
