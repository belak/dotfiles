# Disable global rc files so PATH doesn't get clobbered. We load it manually so
# we can get the global env before we start mucking with it.
setopt noglobalrcs

if [[ -f /etc/zsh/zprofile ]]
then
	source /etc/zsh/zprofile
fi

# Simple platform detection
if [[ `uname` = "Darwin" ]]
then
	BELAK_OSX=true
else
	BELAK_LINUX=true
fi

# Platform specific settings
if [[ -n $BELAK_OSX ]]
then
	export CLICOLOR=1
fi

# Random settings
export EDITOR=vim
export PYTHONDONTWRITEBYTECODE=true
export VIRTUAL_ENV_DISABLE_PROMPT=true

# Path things
export ANDROID_HOME="$HOME/.runtime/android-sdk"
export GOPATH="$HOME/go"
export WORKON_HOME="$HOME/.runtime/python"

export PATH="$GOPATH/bin:/usr/bin/vendor_perl:$HOME/bin:$HOME/.rbenv/bin:/usr/local/bin:$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"

if [[ -d "$HOME/.runtime/go" ]]
then
	export GOROOT="$HOME/.runtime/go"
	export PATH="$GOROOT/bin:$PATH"
fi
