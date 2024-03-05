{
  pkgs ? import <nixpkgs> { },
}:
{
  pyenv = pkgs.callPackage ./pyenv.nix { };
  pyenv-virtualenv = pkgs.callPackage ./pyenv-virtualenv.nix { };
  rbenv = pkgs.callPackage ./rbenv.nix { };
  ruby-build = pkgs.callPackage ./ruby-build.nix { };
  trekscii = pkgs.callPackage ./trekscii.nix { };
  wezterm-bin = pkgs.callPackage ./wezterm-bin.nix { };
}
