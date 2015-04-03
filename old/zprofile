## Misc
# startx on tty1
if [[ `tty` = '/dev/tty1' ]]
then
	exec startx
fi

if [[ -n $BELAK_LINUX ]]
then
	alias ls="ls --color=auto"
	alias grep="grep --color=auto"

	# Remove the extra space - this doesn't appear to work in iTerm, so we only
	# do it for linux.
	ZLE_RPROMPT_INDENT=0
fi

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Aliases
alias json="python -mjson.tool"

# Important defaults
export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'

# Programming defaults
export ANDROID_HOME="$HOME/.runtime/android-sdk"
export GOPATH="$HOME/go"
export WORKON_HOME="$HOME/.runtime/python"
export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'

if [[ -d "$HOME/.runtime/go" ]]
then
	export GOROOT="$HOME/.runtime/go"
	path=(
		"$GOROOT/bin:$PATH"
		$path
	)
fi

# Random settings
export PYTHONDONTWRITEBYTECODE=true

# Make sure we have a locale set
if [[ -z "$LANG" ]]
then
	export LANG='en_US.UTF-8'
fi

## Paths
# bin paths
path=(
	/usr/local/{bin,sbin}
	"$GOPATH/bin"
	"$ANDROID_HOME/tools"
	"$ANDROID_HOME/platform-tools"
	$path
)

# ZSH paths
fpath=(
	"$HOME/.belak/zsh"
	$fpath
)

# Less
export LESS='-g -i -M -R -S -w -z-4'

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] ))
then
	export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi
