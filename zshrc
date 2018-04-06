zstyle ':prezto:load' pmodule \
    'environment' \
    'helper' \
    'editor' \
    'git' \
    'gpg' \
    'ssh' \
    'python' \
    'ssh'
    #'ruby' \

# Load needed repos
[[ ! -d "$HOME/.antigen" ]] && git clone https://github.com/zsh-users/antigen.git "$HOME/.antigen"
source "$HOME/.antigen/antigen.zsh"
[[ ! -d "$HOME/.nvm" ]] && git clone https://github.com/creationix/nvm "$HOME/.nvm"

# Load my plugin library
antigen use belak/zsh-utils

# Load any completions we want before the completion module
antigen bundle zsh-users/zsh-completions

# Load any plugins we want. Note that order matters. Most plugins will complain
# if they're missing dependencies.
antigen bundle editor
antigen bundle history
antigen bundle prompt
antigen bundle utility
antigen bundle completion

# Load any external bundles we want
antigen bundle postmodern/chruby share/chruby/chruby.sh
antigen bundle postmodern/chruby share/chruby/auto.sh
antigen bundle rupa/z z.sh
antigen bundle zsh-users/zsh-syntax-highlighting

antigen apply

# Enable the prompt we want and ensure we're using emacs bindings.
prompt belak
bindkey -e

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

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Load fzf. This will usually be installed when neovim plugins are installed.
#if [[ -f ~/.fzf.zsh ]] source ~/.fzf.zsh

# Make it possible to add per-machine customizations.
if [[ -f ~/.zshrc.local ]] source ~/.zshrc.local

#test -e "${HOME}/.iterm2_shell_integration.zsh" && \
#  source "${HOME}/.iterm2_shell_integration.zsh"
