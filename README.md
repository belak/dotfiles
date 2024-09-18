# belak's dotfiles

## Nix

For setup, run the following:

``` shell
# NixOS setup
sudo ln -sf ~/.dotfiles/nix/flake.nix /etc/nixos/flake.nix
sudo nixos-rebuild switch --flake ~/.dotfiles

# nix-darwin setup
nix run nix-darwin -- switch --flake ~/.dotfiles

# home-manager setup
nix run github:nix-community/home-manager -- switch --flake ~/.dotfiles
```

### Concepts

Hosts are configured with a user-level nix target (`home-manager`) and often a
host-level nix target (`nix-darwin`, `nixos`, etc).

In general, packages should be installed at the user-level unless they need to
be installed at a system level to work properly. This includes things like
system daemons, programs needed to set up the system, and shells.

### Work Computers

Note that the Nix flake in this repo is also used as a dependency in a private
work flake. This allows me to keep most common configuration here, in modules,
and have any work-specific config in a private repo.
