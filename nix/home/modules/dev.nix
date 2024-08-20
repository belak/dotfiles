{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.belak.dev;
in
{
  options.belak.dev = {
    enable = lib.mkEnableOption "devtools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # TODO: most of these packages probably shouldn't be installed globally -
      # they make more sense to install local to projects.

      # Various tools
      cloc
      cmake
      crane
      dos2unix
      editorconfig-core-c
      exiftool
      gnumake
      httpie
      imagemagick
      jq
      lcov
      mdbook
      patchelf
      picotool
      pkg-config
      protobuf
      shellcheck
      shfmt
      stylelint

      # Javascript
      nodePackages.js-beautify

      # Go
      go
      gore
      gotests
      gotools

      # Python
      poetry
      python311
      python3Packages.black
      ruff
      virtualenv
      my.pyenv
      my.pyenv-virtualenv

      # Ruby
      my.rbenv
      my.ruby-build

      # Podman
      podman
      podman-compose

      # Zig
      zig
      zls

      # Packages I'm trying out
      unstable.diskonaut
      unstable.helix
      unstable.jj
      unstable.mdcat
      unstable.nnn
    ];
  };
}
