# belak's dotfiles

belak's personal workstation config: random dotfiles and nix modules which don't
make sense to keep in project configs.

## Setup

``` shell
# nix-darwin
sudo darwin-rebuild --flake ~/.dotfiles switch

# NixOS
sudo nixos-rebuild switch --flake ~/.dotfiles#kuja
```

NixOS hosts need a login password provisioned by hand before the first boot.
See [the user module](nix/nixos/users/belak/README.md).
