{ pkgs, config, lib, ... }:

let
  cfg = config.belak.dev;
in
{
  options.belak.dev = {
    enable = lib.mkEnableOption "devtools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # Various tools
      cloc
      crane
      dos2unix
      editorconfig-core-c
      pkg-config
      protobuf

      # Libraries for projects
      SDL_compat
      SDL_gfx
      SDL_image
      SDL_mixer
      SDL_ttf

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
