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
      crane
      dos2unix
      editorconfig-core-c
      exiftool
      ffmpeg
      fswatch
      grpcurl
      gnumake
      unstable.imagemagick
      unstable.ghostscript
      jq
      just
      patchelf
      yt-dlp

      # Python
      poetry
      ruff
      uv
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
      nix-update
      unstable.helix
      unstable.jujutsu
      unstable.mdcat
    ];
  };
}
