# Path settings need to be done as soon as possible for the startx magic
export GOPATH="$HOME/go"

typeset -U path fpath
path=("$HOME/bin" "$GOPATH/bin" "$HOME/.rbenv/shims" "$HOME/.rbenv/bin" "$HOME/.local/bin" $path)
fpath=("$HOME/.belak/zsh" $fpath)

# Run startx if we're on tty1
if [[ `tty` == "/dev/tty1" ]]; then
    exec startx
fi

zstyle ':prezto:*:*' color 'yes'
zstyle ':prezto:module:editor' key-bindings 'emacs'
zstyle ':prezto:module:prompt' theme 'agnoster'
zstyle ':prezto:module:ruby:chruby' auto-switch 'yes'
zstyle ':prezto:module:terminal' auto-title 'yes'
zstyle ':prezto:module:python' autovenv 'yes'

# Load needed repos
[[ ! -d "$HOME/.antigen" ]] && git clone https://github.com/zsh-users/antigen.git "$HOME/.antigen"
source "$HOME/.antigen/antigen.zsh"
[[ ! -d "$HOME/.nvm" ]] && git clone https://github.com/creationix/nvm "$HOME/.nvm"

# Load the basic prezto library
antigen use prezto

# Load core modules
antigen bundle sorin-ionescu/prezto modules/helper
antigen bundle sorin-ionescu/prezto modules/editor
antigen bundle sorin-ionescu/prezto modules/history
antigen bundle sorin-ionescu/prezto modules/git
antigen bundle sorin-ionescu/prezto modules/prompt
antigen bundle sorin-ionescu/prezto modules/utility

# Load bundled language modules
antigen bundle sorin-ionescu/prezto modules/node
antigen bundle sorin-ionescu/prezto modules/python
antigen bundle sorin-ionescu/prezto modules/ruby

# Load any external bundles we want
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle postmodern/chruby share/chruby/chruby.sh
antigen bundle postmodern/chruby share/chruby/auto.sh
antigen bundle rupa/z z.sh

antigen apply

# Disable SHARE_HISTORY
unsetopt SHARE_HISTORY

# If a default ruby is set, switch to it. If chruby was installed globally, the
# ruby module would trigger this automatically, but because we bootstrap it with
# zgen, that isn't an option.
chruby_auto

# Disable ^s and ^q
stty -ixon

# Aliases
alias json="python -mjson.tool"
alias j="z"
alias prand='cat /usr/share/pokeshell/$(($RANDOM % 151 + 1)).pokemon'

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
export WORKON_HOME="$HOME/.virtualenvs"
export VIRTUAL_ENV_DISABLE_PROMPT=true
export DISABLE_AUTO_UPDATE=true

export DEFAULT_USER=belak

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Load fzf. This will usually be installed when neovim plugins are installed.
[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh

# Make it possible to add per-machine customizations.
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
