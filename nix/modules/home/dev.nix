{ pkgs, ... }: {
  home.packages = with pkgs; [
    # Various tools
    cloc
    crane
    dos2unix
    editorconfig-core-c
    pkg-config

    # Libraries for projects
    SDL_compat
    SDL_gfx
    SDL_image
    SDL_mixer
    SDL_ttf

    # Javascript
    yarn

    # Python
    my.pyenv
    my.pyenv-virtualenv

    # Ruby
    my.rbenv
    my.ruby-build

    # Rust
    unstable.rustc
    unstable.cargo
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
}
