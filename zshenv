export GOPATH="$HOME/go"

typeset -U path fpath
path=(
    "$HOME/bin"
    "/Applications/Inkscape.app/Contents/Resources/bin"
    "/Applications/Postgres.app/Contents/Versions/latest/bin"
    "/Applications/Sublime Text.app/Contents/SharedSupport/bin"
    "/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
    "$GOPATH/bin"
    "$HOME/.cargo/bin"
    "$HOME/.emacs.d/bin"
    "$HOME/.poetry/bin"
    "$HOME/.pyenv/shims"
    "$HOME/.rbenv/shims"
    "$HOME/.rbenv/bin"
    "$HOME/.local/bin"
    /usr/local/opt/python/libexec/bin
    $path
)
fpath=("$HOME/.belak/zsh" $fpath)

# Certain default applications
export ALTERNATE_EDITOR=""
export EDITOR=vim
export PAGER=less

# Magic incantations
export NVIM_TUI_ENABLE_CURSOR_SHAPE=1
export WORKON_HOME="$HOME/.virtualenvs"
export DISABLE_AUTO_UPDATE=true

# It's frustrating that .pyc (and similar files) exist, so it's good that we can
# disable them. This makes python dev much less painful.
export PYTHONDONTWRITEBYTECODE=true

# Improve mouse input with Firefox on Linux
export MOZ_USE_XINPUT2=1

# Most toolkits don't actually scale properly to different DPIs. Alacritty is
# smart enough to do this, but it throws off font sizes, so we force the
# underlying windowing toolkit they use to return a scaling factor of 1.
export WINIT_X11_SCALE_FACTOR=1

# We manually display the venv, so we don't want virtualenv doing that for us
export VIRTUAL_ENV_DISABLE_PROMPT=true
