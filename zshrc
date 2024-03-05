if [[ $+commands[tty] && $(tty) = "/dev/tty1" ]]; then
  exec startx
fi

#
# Plugin Loading Utils
#

# I got so tired of plugin managers changing or going missing every few years,
# so I just wrote this minimal function to handle all the use-cases I need.
belak-load() {
  cleaned_path=${1:l}
  repo=${cleaned_path:h2}
  if [[ ! -d "$HOME/.config/belak/$cleaned_path" ]]; then
    dirname=$(dirname $repo)
    mkdir -p "$HOME/.config/belak/$dirname"
    git clone "https://github.com/$repo" "$HOME/.config/belak/$repo"
  fi
  target=${2:-${cleaned_path:t}.plugin.zsh}

  set --
  source "$HOME/.config/belak/$cleaned_path/$target"
}

#
# Plugins
#

# Specify completions we want before the completion module is loaded
belak-load 'zsh-users/zsh-completions'

# Specify which zsh-utils modules we want
belak-load 'belak/zsh-utils/editor'
belak-load 'belak/zsh-utils/history'
belak-load 'belak/zsh-utils/prompt'
belak-load 'belak/zsh-utils/utility'
belak-load 'belak/zsh-utils/completion'

# Load gitstatus for our prompt
belak-load 'romkatv/gitstatus'

# Specify additional external plugins we want
belak-load 'agkozak/zsh-z'
#belak-load 'rupa/z' 'z.sh'
#belak-load 'z-shell/f-sy-h'

#
# Settings
#

# Set any settings or overrides here - enable the prompt we want and ensure
# we're using emacs bindings.
prompt belak
bindkey -e

# Disable ^s and ^q
stty -ixon

# In order to group dirs together, we need to specify that we're grouping.
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*'         group-name ''

# List dirs first, to match what we do for ls.
zstyle ':completion:*' list-dirs-first true


#
# Aliases
#

alias json="python -mjson.tool"
alias k="kubectl"
alias j="z"

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

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

# Load fzf. This will usually be installed when neovim plugins are installed.
if [[ -f ~/.fzf.zsh ]] source ~/.fzf.zsh

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
