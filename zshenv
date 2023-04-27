export GOPATH="$HOME/go"

# Add a bunch of locations we use to the path. There are a bunch of weird
# caviats here.
#
# On macOS, for some reason, when we use a non-interactive shell /usr/local/bin
# and /usr/local/sbin (which are the default location for homebrew) don't get
# added to the path, so we force it here just in case. This fixes some oddities
# in Emacs.
#
# Most other changes make a bit more sense.
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
    /usr/local/bin
    /usr/local/sbin
    /opt/homebrew/bin
    /opt/homebrew/sbin
    $path
)
fpath=(
    "$HOME/.belak/zsh"
    /opt/homebrew/share/zsh/site-functions
    $fpath
)

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

# Currently, the default window decorations for the windowing toolkit Alacritty
# uses don't fit in at all. Unfortunately, until the proper Wayland protocol
# extension is implemented, the easiest way to fix this is to force the use of
# x11 in the underlying toolkit to trick it into relying on server-side decorations.
export WINIT_UNIX_BACKEND=x11

# We manually display the venv, so we don't want virtualenv doing that for us
export VIRTUAL_ENV_DISABLE_PROMPT=true

# Workaround for some SSH flags I use - they're deprecated on macOS for some
# reason, but this will fix the warning for now.
export APPLE_SSH_ADD_BEHAVIOR=openssh

# Load cargo env if available
if [[ -f "$HOME/.cargo/env" ]]; then source "$HOME/.cargo/env"; fi
