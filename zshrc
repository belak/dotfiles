# This is loosely based on the grml-zsh-config
#
# Many things were taken from there, but a number of assumptions were made and
# so many extra checks and unneeded features have been removed.

# Simple OS detection
OSTYPE=$(uname -s)

islinux() {
	[[ $OSTYPE == "Linux" ]]
}

isdarwin() {
	[[ $OSTYPE == "Darwin" ]]
}

# Do our best to load virtualenvwrapper
if which virtualenvwrapper.sh &>/dev/null
then
	source $(which virtualenvwrapper.sh)
fi

# Aliases
alias json="python -mjson.tool"

# Disable ^s and ^q
stty -ixon

# ZSH options

# Always append to the hist file
setopt append_history

# Get new commands from the histfile in other sessions
setopt share_history

# Store timestamp and duration to the history file
setopt extended_history

# Only store one instance of each command
setopt histignorealldups

# Don't add commands to history that started with space
setopt histignorespace

# Display pid when suspending processes
setopt longlistjobs

# Report status of background jobs immediately
setopt notify

# No beeping
set nobeep

# Try to get color for ls and grep
typeset -ga ls_options
typeset -ga grep_options
if ls --color=auto / >/dev/null 2>&1; then
    ls_options=(--color=auto)
elif ls -G / >/dev/null 2>&1; then
    ls_options=(-G)
fi
if grep --color=auto -q "a" <<< "a" >/dev/null 2>&1; then
    grep_options=(--color=auto)
fi

# Set some important env vars
export EDITOR=vim
export PAGER=less
export PYTHONDONTWRITEBYTECODE=true

# Set up dircolors
dircolors &>/dev/null && eval $(dircolors -b)

# Stuff for osx
isdarwin && export CLICOLOR=1

# Support colors in less
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# Automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# Golang stuff
export GOPATH=$HOME/go

# Add our custom stuff
fpath=("$HOME/.belak/zsh" $fpath)
path=("$HOME/bin" "$GOPATH/bin" $path)

# Load some useful zsh modules
zmodload -i zsh/mathfunc
zmodload -i zsh/parameter

# ZSH completion magic

autoload -U compinit
compinit -d $HOME/.zcompdump

# allow one error for every three characters typed in approximate completer
zstyle ':completion:*:approximate:'    max-errors 'reply=( $((($#PREFIX+$#SUFFIX)/3 )) numeric )'

# start menu completion only if it could find no unambiguous initial string
zstyle ':completion:*:correct:*'       insert-unambiguous true
zstyle ':completion:*:corrections'     format $'%{\e[0;31m%}%d (errors: %e)%{\e[0m%}'
zstyle ':completion:*:correct:*'       original true

# activate color-completion
zstyle ':completion:*:default'         list-colors ${(s.:.)LS_COLORS}

# format on completion
zstyle ':completion:*:descriptions'    format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'

# match uppercase from lowercase
zstyle ':completion:*'                 matcher-list 'm:{a-z}={A-Z}'

# separate matches into groups
zstyle ':completion:*:matches'         group 'yes'
zstyle ':completion:*'                 group-name ''

zstyle ':completion:*:messages'        format '%d'
zstyle ':completion:*:options'         auto-description '%d'

# describe options in full
zstyle ':completion:*:options'         description 'yes'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# provide verbose completion information
zstyle ':completion:*'                 verbose true

# recent (as of Dec 2007) zsh versions are able to provide descriptions
# for commands (read: 1st word in the line) that it will list for the user
# to choose from. The following disables that, because it's not exactly fast.
zstyle ':completion:*:-command-:*:'    verbose false

# set format for warnings
zstyle ':completion:*:warnings'        format $'%{\e[0;31m%}No matches for:%{\e[0m%} %d'

# define files to ignore for zcompile
zstyle ':completion:*:*:zcompile:*'    ignored-patterns '(*~|*.zwc)'
zstyle ':completion:correct:'          prompt 'correct to: %e'

# Ignore completion functions for commands you don't have:
zstyle ':completion::(^approximate*):*:functions' ignored-patterns '_*'

# Provide more processes in completion of programs like killall:
zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'

# complete manual by their section
zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true
zstyle ':completion:*:man:*'      menu yes select

# Search path for sudo completion
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin \
	/usr/local/bin  \
	/usr/sbin       \
	/usr/bin        \
	/sbin           \
	/bin            \
	/usr/X11R6/bin

# caching
#[[ -d $ZSHDIR/cache ]] && zstyle ':completion:*' use-cache yes && \
#	zstyle ':completion::complete:*' cache-path $ZSHDIR/cache/

# use generic completion system for programs not yet defined; (_gnu_generic works
# with commands that provide a --help option with "standard" gnu-like output.)
for compcom in cp deborphan df feh fetchipac gpasswd head hnb ipacsum mv pal stow uname; do
	[[ -z ${_comps[$compcom]} ]] && compdef _gnu_generic ${compcom}
done
unset compcom

# Fix key bindings
bindkey -e

typeset -A key
key=(
    Home     "${terminfo[khome]}"
    End      "${terminfo[kend]}"
    Insert   "${terminfo[kich1]}"
    Delete   "${terminfo[kdch1]}"
    Up       "${terminfo[kcuu1]}"
    Down     "${terminfo[kcud1]}"
    Left     "${terminfo[kcub1]}"
    Right    "${terminfo[kcuf1]}"
    PageUp   "${terminfo[kpp]}"
    PageDown "${terminfo[knp]}"
    BackTab  "${terminfo[kcbt]}"
)

[[ -n "${key[Home]}"   ]] && bindkey "${key[Home]}"   beginning-of-line
[[ -n "${key[End]}"    ]] && bindkey "${key[End]}"    end-of-line
[[ -n "${key[Insert]}" ]] && bindkey "${key[Insert]}" overwrite-mode
[[ -n "${key[Delete]}" ]] && bindkey "${key[Delete]}" delete-char
[[ -n "${key[Up]}"     ]] && bindkey "${key[Up]}"     up-line-or-history
[[ -n "${key[Down]}"   ]] && bindkey "${key[Down]}"   down-line-or-history
[[ -n "${key[Left]}"   ]] && bindkey "${key[Left]}"   backward-char
[[ -n "${key[Right]}"  ]] && bindkey "${key[Right]}"  forward-char

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
function zle-line-init () {
	if [[ -n $DISPLAY ]]; then
		echoti smkx
	fi
}
function zle-line-finish () {
	if [[ -n $DISPLAY ]]; then
		echoti rmkx
	fi
}

zle -N zle-line-init
zle -N zle-line-finish

# Magic moving
autoload -U zmv

# Important histfile stuff
HISTFILE=$HOME/.histfile
HISTSIZE=5000
SAVEHIST=10000

# "persistent history"
if [[ -r ~/.important_commands ]]; then
    fc -R ~/.important_commands
fi

# do we have GNU ls with color-support?
if [[ "$TERM" != dumb ]]; then
    #a1# List files with colors (\kbd{ls -F \ldots})
    alias ls='command ls -F '${ls_options:+"${ls_options[*]}"}
    #a1# List all files, with colors (\kbd{ls -la \ldots})
    alias la='command ls -la '${ls_options:+"${ls_options[*]}"}
    #a1# List files with long colored list, without dotfiles (\kbd{ls -l \ldots})
    alias ll='command ls -l '${ls_options:+"${ls_options[*]}"}
    #a1# List files with long colored list, human readable sizes (\kbd{ls -hAl \ldots})
    alias lh='command ls -hAl '${ls_options:+"${ls_options[*]}"}
    #a1# List files with long colored list, append qualifier to filenames (\kbd{ls -lF \ldots})\\&\quad(\kbd{/} for directories, \kbd{@} for symlinks ...)
    alias l='command ls -lF '${ls_options:+"${ls_options[*]}"}

    alias grep='grep '${grep_options:+"${grep_options[*]}"}
    alias egrep='egrep '${grep_options:+"${grep_options[*]}"}
else
    alias ls='command ls -F'
    alias la='command ls -la'
    alias ll='command ls -l'
    alias lh='command ls -hAl'
    alias l='command ls -lF'
fi

# TODO: Simple extract

# Simple jump command
function j {
	usage=false

	if [[ $# > 0 ]]; then
		arg=$1
		case "$1" in
			"a") arg="add" ;;
			"d") arg="del" ;;
			"j") arg="jump" ;;
		esac

		case "$arg" in
			"--help"|"-h")
				usage=true
				;;
			"add"|"del"|"jump")
				act=$arg
				shift
				if [[ $# > 0 ]]; then
					dir=$1
				else
					usage=true
				fi
				;;
			*)
				act="jump"
				dir=$1
				;;
		esac
	else
		usage=true
	fi

	if [[ $usage != true ]]; then
		if [[ $act = "add" && -e "$HOME/.belak/j/$dir" ]]; then
			msg="jump dir already exists"
			usage=true
		elif [[ $act != "add" && ! -e "$HOME/.belak/j/$dir" ]]; then
			msg="jump dir doesn't exist"
			usage=true
		fi
	fi

	if [[ $usage = true ]]; then
		[[ -n $msg ]] && echo $msg
		echo 'j [add|jump|del] name'
		return
	fi

	case "$act" in
		"add")
			echo "cd '$(pwd)'" > "$HOME/.belak/j/$dir"
			chmod +x "$HOME/.belak/j/$dir"
			;;
		"del")
			rm "$HOME/.belak/j/$dir"
			;;
		"jump")
			source "$HOME/.belak/j/$dir"
			;;
	esac
}

autoload -U promptinit
promptinit

# Actually set the prompt
prompt gentoo
