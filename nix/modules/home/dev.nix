{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.belak.dev;
in {
  options.belak.dev = {
    enable = lib.mkEnableOption "devtools";
  };

  config = lib.mkIf cfg.enable {
    # programs.java.enable = true;

    home.packages = with pkgs; [
      # Various tools
      android-tools
      clang
      cloc
      crane
      dos2unix
      editorconfig-core-c
      exiftool
      gnumake
      imagemagick
      picotool
      pkg-config
      protobuf

      # Libraries for projects
      SDL_compat
      SDL_gfx
      SDL_image
      SDL_mixer
      SDL_ttf
      zlib.dev

      # Java
      jdk8

      # Javascript
      yarn

      # Python
      python311
      virtualenv
      my.pyenv
      my.pyenv-virtualenv

      # Ruby
      my.rbenv
      my.ruby-build

      # Rust
      unstable.rustc
      unstable.rustfmt
      unstable.cargo
      unstable.clippy
      unstable.rust-analyzer

      # Go and additional tools
      unstable.go
      unstable.gofumpt
      unstable.golangci-lint
      unstable.goreleaser
      unstable.gotools
      unstable.protoc-gen-go
      unstable.protoc-gen-go-grpc
    ];
  };
}
