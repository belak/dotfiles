{ pkgs, ... }: {
  home.packages = with pkgs; [
    binutils
    curl
    dig
    fd
    findutils
    fzf
    git
    htop
    jq
    killall
    pwgen
    ripgrep
    tmux
    wget
  ];
}
