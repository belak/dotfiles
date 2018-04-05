export GOPATH="$HOME/go"

typeset -U path fpath
path=("$HOME/bin" "$GOPATH/bin" "$HOME/.cargo/bin" "$HOME/.rbenv/shims" "$HOME/.rbenv/bin" "$HOME/.local/bin" /usr/local/opt/python/libexec/bin $path)
fpath=("$HOME/.belak/zsh" $fpath)

# Magic incantations
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -c"
export PAGER=less
export PYTHONDONTWRITEBYTECODE=true
export NVIM_TUI_ENABLE_CURSOR_SHAPE=1
export WORKON_HOME="$HOME/.virtualenvs"
export VIRTUAL_ENV_DISABLE_PROMPT=true
export DISABLE_AUTO_UPDATE=true