{ pkgs, ... }: {
  home.packages = with pkgs; [
    binutils
    curl
    dig
    dmidecode
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
