#
# Graphical Session
#

if [[ $(tty) == "/dev/tty1" ]]; then
    exec niri-session
fi

#
# Plugin Manager
#

ANTIDOTE_DIR=${ZDOTDIR:-~}/.antidote

if [[ ! -d ${ANTIDOTE_DIR} ]]; then
  git clone --depth=1 https://github.com/mattmc3/antidote.git ${ANTIDOTE_DIR}
fi

source ${ANTIDOTE_DIR}/antidote.zsh

# Load plugins from ~/.zsh_plugins.txt
antidote load

#
# Settings
#

# Set up our prompt and hide the rprompt after commands complete
promptinit && prompt belak
setopt transient_rprompt

# Enable emacs keybinds
bindkey -e

# List dirs first, to match what we do for ls.
zstyle ':completion:*' list-dirs-first true

# Remove _approximate from completers, as I don't ever want things to correct to
# something I didn't type.
zstyle ':completion:*' completer _complete _match

# Disable magic enter, for a similar reason to why I disable the _approximate
# completer.
zstyle ':zephyr:plugin:editor' 'magic-enter' false

# Unset special-dirs so `..` doesn't show up in completions.
zstyle -d ':completion:*' special-dirs

# Set the default Less options.
export LESS='-F -g -i -M -R -S -w -z-4'

#
# Aliases
#

alias json="python -mjson.tool"
alias k="kubectl"

if (( $+commands[nvim] )); then
  alias vim="nvim"
fi

#
# Functions
#

gocover () {
  t="/tmp/go-cover.$$.tmp"
  go test -coverprofile=$t $@ && go tool cover -html=$t && unlink $t
}

#
# Additional Bundles
#

if (( $+commands[fzf] )); then
  eval "$(fzf --zsh)"
fi

if (( $+commands[jump] )); then
  eval "$(jump shell)"
fi

if (( $+commands[pyenv] )); then
  eval "$(pyenv init -)"
fi

if (( $+commands[pyenv-virtualenv-init] )); then
  eval "$(pyenv virtualenv-init -)"
fi

if (( $+commands[rbenv] )); then
  eval "$(rbenv init -)"
fi

if (( $+commands[direnv] )); then
  eval "$(direnv hook zsh)"
fi

# Make it possible to add per-machine customizations.
if [[ -f ~/.zshrc.local ]] source ~/.zshrc.local

#if [[ -f ~/.iterm2_shell_integration.zsh ]] source ~/.iterm2_shell_integration.zsh
