# PATH additions

# Personal utils and scripts
fish_add_path --path --prepend \
    $HOME/bin

# Application binaries
fish_add_path --path \
    /Applications/Ghostty.app/Contents/MacOS \
    /Applications/Postgres.app/Contents/Versions/latest/bin \
    "/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

# Development tools
fish_add_path --path \
    $GOPATH/bin \
    $VOLTA_HOME/bin \
    $HOME/.cargo/bin \
    $HOME/.poetry/bin
