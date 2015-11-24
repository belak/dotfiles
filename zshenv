# These are env things. It's here and not in the zshrc so emacs and
# exec-path-from-shell can grab things without requiring the entire shellrc to
# be loaded.

# Automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# Golang stuff
export GOPATH=$HOME/go

# Add our custom stuff
fpath=("$HOME/.belak/zsh" $fpath)
path=("$HOME/bin" "$GOPATH/bin" "$HOME/.rbenv/shims" "$HOME/.rbenv/bin" "$HOME/.local/bin" $path)
if which ruby >/dev/null && which gem >/dev/null; then
	path=("$(ruby -rubygems -e 'puts Gem.user_dir')/bin" $path)
fi
