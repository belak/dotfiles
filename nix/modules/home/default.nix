inputs: {
  # NOTE: We specifically don't call these modules - this is needed so they can
  # be imported in flakes which depend on this one. They are specifically meant
  # to be called as home-manager modules. They may rely on additional setup from
  # self.lib.mkDarwinSystem or self.lib.mkHome

  common = import ./common.nix;

  # Platforms
  darwin = import ./darwin.nix;
  linux = import ./linux.nix;

  # Feature modules
  dotfiles = import ./dotfiles.nix;
  go = import ./go.nix;
  rust = import ./rust.nix;
}
