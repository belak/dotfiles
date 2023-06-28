# belak's dotfiles

## NixOS

For setup, run the following:

``` shell
# NixOS setup
sudo ln -sf ~/.dotfiles/nix/nixos/configuration.nix /etc/nixos/configuration.nix
sudo nixos-rebuild switch

# home-manager setup
nix-channel --add https://github.com/nix-community/home-manager/archive/release-23.05.tar.gz home-manager
nix-channel --update
home-manager switch -f ~/.dotfiles/nix/home-manager/home.nix
```
