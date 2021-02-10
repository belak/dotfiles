if [[ $+commands[tty] && $(tty) = "/dev/tty1" ]]; then
    exec startx
fi

#
# Dependencies
#

[[ ! -d "$HOME/.zplug" ]] && git clone https://github.com/zplug/zplug "$HOME/.zplug"
source "$HOME/.zplug/init.zsh"
[[ ! -d "$HOME/.nvm" ]] && git clone https://github.com/creationix/nvm "$HOME/.nvm"

#
# Plugins
#

# Let zplug manage itself when using zplug update
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# Specify completions we want before the completion module
zplug "zsh-users/zsh-completions"

# Specify which zsh-utils modules we want
zplug "belak/zsh-utils", use:"editor/*.plugin.zsh"
zplug "belak/zsh-utils", use:"history/*.plugin.zsh"
zplug "belak/zsh-utils", use:"prompt/*.plugin.zsh"
zplug "belak/zsh-utils", use:"utility/*.plugin.zsh"
zplug "belak/zsh-utils", use:"completion/*.plugin.zsh"

# Load gitstatus for our prompt
zplug "romkatv/gitstatus"

# Specify additional external plugins we want
zplug "rupa/z", use:z.sh
#zplug "zsh-users/zsh-syntax-highlighting"
zplug "zdharma/fast-syntax-highlighting", defer:2

if ! zplug check; then
    zplug install
fi

zplug load

#
# Settings
#

# Set any settings or overrides here - enable the prompt we want and ensure
# we're using emacs bindings.
prompt belak
bindkey -e

# Disable ^s and ^q
stty -ixon

#
# Aliases
#

alias json="python -mjson.tool"
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

# Make it possible to add per-machine customizations.
if [[ -f ~/.zshrc.local ]] source ~/.zshrc.local

#if [[ -f ~/.iterm2_shell_integration.zsh ]] source ~/.iterm2_shell_integration.zsh
