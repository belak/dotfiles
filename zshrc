#
# Dependencies
#

[[ ! -d "$HOME/.antigen" ]] && git clone https://github.com/zsh-users/antigen.git "$HOME/.antigen"
source "$HOME/.antigen/antigen.zsh"
[[ ! -d "$HOME/.nvm" ]] && git clone https://github.com/creationix/nvm "$HOME/.nvm"

#
# Plugins
#

if [[ -f "$HOME/.use-prezto" ]]; then
  #source "$HOME/.antigen/bundles/sorin-ionescu/prezto/modules/prompt/external/powerlevel10k/config/p10k-lean.zsh"
  zstyle ':prezto:*:*' color 'yes'
  zstyle ':prezto:module:editor' key-bindings 'emacs'
  zstyle ':prezto:module:git:alias' skip 'yes'
  #zstyle ':prezto:module:prompt' theme 'powerlevel10k'
  zstyle ':prezto:module:prompt' theme 'belak'
  zstyle ':prezto:module:prompt' pwd-length 'short'
  zstyle ':prezto:module:ruby:chruby' auto-switch 'yes'
  zstyle ':prezto:module:terminal' auto-title 'yes'
  zstyle ':prezto:module:python' autovenv 'yes'
  zstyle ':prezto:load' pmodule \
      'environment' \
      'helper' \
      'editor' \
      'history' \
      'git' \
      'contrib-prompt' \
      'prompt' \
      'utility' \
      'gpg' \
      'ssh' \
      'python' \
      'ruby' \
      'completion' \
      'syntax-highlighting' \
      'ssh'

  antigen use prezto
else
  # Set the default plugin repo to be zsh-utils
  antigen use belak/zsh-utils

  # Specify completions we want before the completion module
  antigen bundle zsh-users/zsh-completions

  # Specify plugins we want
  antigen bundle editor
  antigen bundle history
  antigen bundle prompt
  antigen bundle utility
  antigen bundle completion
fi

# Specify additional external plugins we want
antigen bundle postmodern/chruby share/chruby/chruby.sh
antigen bundle postmodern/chruby share/chruby/auto.sh
antigen bundle rupa/z z.sh
#antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zdharma/fast-syntax-highlighting

# Load everything
antigen apply

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

# Alias vim to nvim if neovim is installed
if which nvim &>/dev/null; then
  alias vim=nvim
fi

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

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

# Load virtualenvwrapper if it exists on the system
if (( $+commands[virtualenvwrapper_lazy.sh] )); then
  source "${commands[virtualenvwrapper_lazy.sh]}"
fi

# If a default ruby is set, switch to it. If chruby was installed globally, the
# ruby module would trigger this automatically, but because we bootstrap it with
# antigen, that isn't an option.
chruby_auto

# Make it possible to add per-machine customizations.
if [[ -f ~/.zshrc.local ]] source ~/.zshrc.local

#if [[ -f ~/.iterm2_shell_integration.zsh ]] source ~/.iterm2_shell_integration.zsh
