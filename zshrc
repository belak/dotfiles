# Path settings need to be done as soon as possible for the startx magic
export GOPATH="$HOME/go"

typeset -U path fpath
path=("$HOME/bin" "$GOPATH/bin" "$HOME/.rbenv/shims" "$HOME/.rbenv/bin" "$HOME/.local/bin" $path)
fpath=("$HOME/.belak/zsh" $fpath)

# Run startx if we're on tty1
if [[ `tty` == "/dev/tty1" ]]; then
    exec startx
fi

# Any zprezto settings need to come before zgen is loaded. Most of these come
# from the default zpreztorc.
zstyle ':prezto:*:*' color 'yes'
zstyle ':prezto:module:editor' key-bindings 'emacs'
zstyle ':prezto:module:prompt' theme 'belak'
zstyle ':prezto:module:ruby:chruby' auto-switch 'yes'
zstyle ':prezto:module:terminal' auto-title 'yes'

# Load needed repos
[[ ! -d "$HOME/.zgen" ]] && git clone https://github.com/tarjoilija/zgen "$HOME/.zgen"
source "$HOME/.zgen/zgen.zsh"
[[ ! -d "$HOME/.nvm" ]] && git clone https://github.com/creationix/nvm "$HOME/.nvm"

# Bootstrap zgen
if ! zgen saved; then
    # We use prezto in place of oh-my-zsh because it's quite a bit simpler and
    # makes some decisions with the default config which I prefer.
    zgen prezto

    # Load some better language support
    zgen prezto node
    zgen prezto python
    zgen prezto ruby
    zgen load postmodern/chruby share/chruby/chruby.sh
    zgen load postmodern/chruby share/chruby/auto.sh

    # Additional plugins
    zgen prezto git
    zgen prezto syntax-highlighting
    #zgen load rupa/z z.sh

    # Save everything we've loaded so far
    zgen save
fi

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
#alias j="z"

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

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Load fzf. This will usually be installed when neovim plugins are installed.
[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh

# Autojump will be in one of a few places depending on the system, so try to
# load it from each of them.
[[ -f /etc/profile.d/autojump.zsh ]] && source /etc/profile.d/autojump.zsh
[[ -f /usr/local/etc/profile.d/autojump.sh ]] && source /usr/local/etc/profile.d/autojump.sh

# Make it possible to add per-machine customizations.
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
