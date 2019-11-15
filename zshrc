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

  # Load gitstatus for our prompt
  antigen bundle romkatv/gitstatus
fi

# Specify additional external plugins we want
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
