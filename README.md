# belak's dotfiles

## NixOS

For setup, run the following:

``` shell
# NixOS setup
sudo ln -sf ~/.dotfiles/nix/flake.nix /etc/nixos/flake.nix
sudo nixos-rebuild switch

# TODO: nix-darwin setup

# home-manager setup
nix-channel --add https://github.com/nix-community/home-manager/archive/release-23.05.tar.gz home-manager
nix-channel --update
home-manager switch -f ~/.dotfiles/nix
```

### Concepts

Each setup (nix-darwin, home-manager, etc) has multiple different ways to
install programs. Each option has at least one way to install programs at a
system level, and one way to install at a user-level.

In general, packages should be installed at the user-level unless they need to
be installed at a system level. The main exceptions to user-level packages are
macOS Apps (so they're properly picked up by Alfred), system daemons, programs
needed to set up the system, and shells.

There should only be a single entrypoint per system - if a system runs on NixOS,
standalone home-manager should not be used. This allows us to keep packages in
sync between global installations and home-manager installations.
