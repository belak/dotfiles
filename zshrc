# Path settings need to be done as soon as possible for the startx magic
export GOPATH="$HOME/go"

typeset -U path
path=("$HOME/bin" "$GOPATH/bin" "$HOME/.rbenv/shims" "$HOME/.rbenv/bin" "$HOME/.local/bin" $path)

export WORKON_HOME="$HOME/.virtualenvs"

# Run startx if we're on tty1
if [[ `tty` == "/dev/tty1" ]]; then
	exec startx
fi

# Load zgen
[[ ! -d "$HOME/.zgen" ]] && git clone https://github.com/tarjoilija/zgen "$HOME/.zgen"
source "$HOME/.zgen/zgen.zsh"

# Bootstrap zgen
if ! zgen saved; then
	# oh-my-zsh is pretty nice, but it tends to do too much. By using zgen to
	# load it, we get a much more minimal base to work from.
	zgen oh-my-zsh

	# Load some better language support
	zgen oh-my-zsh plugins/golang
	zgen oh-my-zsh plugins/virtualenvwrapper
	zgen load postmodern/chruby share/chruby/chruby.sh

	# Various dev tools
	zgen oh-my-zsh plugins/emacs
	zgen load rupa/z z.sh
	zgen oh-my-zsh plugins/z

	# Load the theme
	zgen oh-my-zsh themes/wezm

	# Additional plugins
	zgen oh-my-zsh plugins/pass
	zgen oh-my-zsh plugins/sudo
	zgen load zsh-users/zsh-syntax-highlighting

	# Load a prompt
	zgen save
fi

# Extra theme settings. This just changes the dirty indicator with a non-unicode
# character because it looks bad in terminus.
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%})%{$fg[red]%}*%{$reset_color%}"

# Disable ^s and ^q
stty -ixon

# Aliases
alias json="python -mjson.tool"
alias j="z"

# Env vars
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -c"
export PAGER=less
export PYTHONDONTWRITEBYTECODE=true
