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
      grpcurl
      gnumake
      httpie
      imagemagick
      jq
      lcov
      mdbook
      patchelf
      picotool
      protobuf
      shellcheck
      shfmt
      stylelint

      # Python
      poetry
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

      # Packages I'm trying out
      codeowners
      git-codeowners
      unstable.diskonaut
      unstable.helix
      unstable.jj
      unstable.mdcat
    ];
  };
}
