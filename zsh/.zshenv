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
export PATH="$GOROOT/bin:$GOPATH/bin:/usr/bin/vendor_perl:$HOME/bin:/usr/local/bin:$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"
