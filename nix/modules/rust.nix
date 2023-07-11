{ pkgs, ... }: {
  home.packages = with pkgs; [
    # Core Language
    unstable.rustc
    unstable.cargo

    # Additional Tools
    unstable.rust-analyzer
  ];
}
