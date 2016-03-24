# Path settings need to be done as soon as possible for the startx magic
export GOPATH="$HOME/go"

typeset -U path fpath
path=("$HOME/bin" "$GOPATH/bin" "$HOME/.rbenv/shims" "$HOME/.rbenv/bin" "$HOME/.local/bin" $path)
fpath=("$HOME/.belak/zsh" $fpath)

export WORKON_HOME="$HOME/.virtualenvs"

# Run startx if we're on tty1
if [[ `tty` == "/dev/tty1" ]]; then
	exec startx
fi

# Any zprezto settings need to come before zgen is loaded. Most of these come
# from the default zpreztorc.
zstyle ':prezto:*:*' color 'yes'
zstyle ':prezto:module:editor' key-bindings 'emacs'
zstyle ':prezto:module:prompt' theme 'sorin'
zstyle ':prezto:module:ruby:chruby' auto-switch 'yes'
zstyle ':prezto:module:terminal' auto-title 'yes'

# Load zgen
[[ ! -d "$HOME/.zgen" ]] && git clone https://github.com/tarjoilija/zgen "$HOME/.zgen"
source "$HOME/.zgen/zgen.zsh"

# Bootstrap zgen
if ! zgen saved; then
    # We use prezto in place of oh-my-zsh because it's quite a bit simpler and
    # makes some decisions with the default config which I prefer.
	zgen prezto

	# Load some better language support
    zgen prezto python
    zgen prezto ruby
	zgen load postmodern/chruby share/chruby/chruby.sh
	zgen load postmodern/chruby share/chruby/auto.sh

	# Additional plugins
    zgen prezto git
    zgen prezto syntax-highlighting
	zgen load rupa/z z.sh

	# Save everything we've loaded so far
	zgen save
fi

# If a default ruby is set, switch to it. If chruby was installed globally, the
# ruby module would trigger this automatically, but because we bootstrap it with
# zgen, that isn't an option.
chruby_auto

# Disable ^s and ^q
stty -ixon

# Aliases
alias json="python -mjson.tool"
alias j="z"

# Alias vim to nvim if neovim is installed
if which nvim &>/dev/null; then
	alias vim=nvim
fi

# Magic incantations
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -c"
export PAGER=less
export PYTHONDONTWRITEBYTECODE=true
export NVIM_TUI_ENABLE_CURSOR_SHAPE=1

[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh
