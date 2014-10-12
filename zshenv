# Disable global rc files so PATH isn't clobbered
setopt noglobalrcs
if [[ -f /etc/zsh/zprofile ]]
then
	source /etc/zsh/zprofile
fi

## Platform Detection
if [[ `uname` = "Darwin" ]]
then
	BELAK_OSX=true
else
	BELAK_LINUX=true
fi

## Non-Global Exports
if [[ -n $BELAK_OSX ]]
then
	export CLICOLOR=1
fi

## Path stuff
export ANDROID_HOME="$HOME/.runtime/android-sdk"
export GOROOT="$HOME/.runtime/go"
export GOPATH="$HOME/go"
export WORKON_HOME="$HOME/.runtime/python"
export EDITOR=vim
export PYTHONDONTWRITEBYTECODE=true

if which rbenv &>/dev/null
then
	eval "$(rbenv init -)"
fi

# Allows local gems to be included in the $PATH
#if which ruby &>/dev/null
#then
#	export PATH="$(ruby -rubygems -e "puts Gem.user_dir")/bin:$PATH"
#fi

# Heroku toolbelt
if [[ -d /usr/local/heroku/bin ]]
then
	export PATH="/usr/local/heroku/bin:$PATH"
fi

# Adds stuff for golang and android
export PATH="$GOROOT/bin:$GOPATH/bin:/usr/bin/vendor_perl:$HOME/bin:$HOME/.rbenv/bin:/usr/local/bin:$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"
