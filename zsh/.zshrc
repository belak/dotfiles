# Belak's zshrc

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

export GOROOT="$HOME/.runtime/go"
export GOPATH="$HOME/go"
export WORKON_HOME="$HOME/.runtime/python"

# Stuff for ruby dev
if which rbenv &>/dev/null
then
	#eval $(rbenv init -)
	export PATH="/Users/belak/.rbenv/shims:${PATH}"
	source "/Users/belak/.rbenv/libexec/../completions/rbenv.zsh"
	rbenv rehash 2>/dev/null
	rbenv() {
		typeset command
		command="$1"
		if [ "$#" -gt 0 ]
		then
		shift
		fi

		case "$command" in
			rehash|shell)
				eval "`rbenv "sh-$command" "$@"`";;
			*)
				command rbenv "$command" "$@";;
		esac
	}
fi

if which virtualenvwrapper.sh &>/dev/null
then
	source $(which virtualenvwrapper.sh)
fi

# Stuff that should only be defined once, such as PATH
if [[ -z $BELAK_DEFINED ]]
then
	export PATH="$HOME/.rbenv/bin:$GOROOT/bin:$GOPATH/bin:$HOME/bin:/usr/local/bin:$PATH"

	## startx on tty1
	if [[ `tty` = '/dev/tty1' ]]
	then
		exec startx
	fi

	BELAK_DEFINED=true
fi

## Magical stuff
. $HOME/.dotfiles/base16-shell/base16-tomorrow.dark.sh

## Alias cmds
alias df="df -h"
alias du="du -h"

alias irc="ssh -t quigley.coded.io tmux attach -t irc"

if [[ -n $BELAK_LINUX ]]
then
	alias ls="ls --color=auto"
	alias grep="grep --color=auto"
fi

# TODO: cleanup after here
# Disable ^s and ^q
stty -ixon

## ZSH Specific Settings
fpath=("$HOME/.belak/zsh" $fpath)

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt histverify
setopt listpacked
unsetopt beep

setopt histignorealldups
setopt histignorespace
setopt incappendhistory
setopt appendhistory

# Some completion stuff
setopt extendedglob
setopt completeinword

# Needed for prompt later
setopt promptsubst
setopt promptpercent

## Completion and prompt
zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' squeeze-slashes true

# Gentoo workaround for sudo path
if [[ -f /etc/gentoo-release ]]
then
	zstyle ':completion:*:sudo:*' command-path "${path[@]}" /usr/local/sbin /usr/sbin /sbin
fi

zstyle :compinstall filename '/home/belak/.zshrc'

## Load all the modules for later
autoload -Uz compinit vcs_info colors
compinit
colors

## Prompt stuff
zstyle ':vcs_info:hg:*' hgrevformat '%r'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' enable git hg svn
zstyle ':vcs_info:*' stagedstr '*'
zstyle ':vcs_info:*' unstagedstr '*'
zstyle ':vcs_info:*' branchformat '%b|%r'
zstyle ':vcs_info:*' actionformats "%F{blue}[ %F{yellow}%c%F{red}%u%F{white}[%{yellow}%b|%a %F{blue}]%f"
zstyle ':vcs_info:*' formats "%F{blue}[ %F{yellow}%c%F{red}%u%F{yellow}%b %F{blue}]%f"

function precmd {
	vcs_info
}

function ssh_prompt {
	[[ -n $SSH_CONNECTION ]] && echo "%F{blue}[ %F{red}ssh %F{blue}]%f"
}

function prompt_start_color {
	if [[ $? != 0 ]]
	then
		echo "%F{red}"
	else
		echo "%F{yellow}"
	fi
}

# Based on hostname, setup the prompt start character
case `hostname` in
	'skeeve')
		prompt_start_char='Ξ'
		;;
	'aahz')
		prompt_start_char='C:\'
		;;
	*'.mtu.edu')
		prompt_start_char='∴'
		;;
	'gleep')
		prompt_start_char='☿'
		;;
	'winslow')
		prompt_start_char='ω'
		;;
	*)
		prompt_start_char=''
		;;
esac

PROMPT='$(prompt_start_color)${prompt_start_char} %F{green}%2c%F{blue} [%f '
RPROMPT='%F{blue}]${vcs_info_msg_0_}%f$(ssh_prompt)%f'

[[ -f "$GOROOT/misc/zsh/go" ]] && source "$GOROOT/misc/zsh/go"

## Key Bindings
bindkey -e
typeset -A key

# Get Terminfo
key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

# setup keys accordingly
[[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
function zle-line-init () {
	if [[ -n $DISPLAY ]]
	then
		echoti smkx
	fi
}
function zle-line-finish () {
	if [[ -n $DISPLAY ]]
	then
		echoti rmkx
	fi
}

zle -N zle-line-init
zle -N zle-line-finish

function j {
	usage=false

	if [[ $# > 0 ]]
	then
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
				if [[ $# > 0 ]]
				then
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

	if [[ $usage != true ]]
	then
		if [[ $act = "add" && -e "$HOME/.belak/j/$dir" ]]
		then
			msg="jump dir already exists"
			usage=true
		elif [[ $act != "add" && ! -e "$HOME/.belak/j/$dir" ]]
		then
			msg="jump dir doesn't exist"
			usage=true
		fi
	fi

	if [[ $usage = true ]]
	then
		[[ -n $msg ]] && echo $msg
		echo 'j [add|jump|del] name'
		return
	fi

	case "$act" in
		"add")
			ln -s "$(pwd)" "$HOME/.belak/j/$dir"
			;;
		"del")
			rm "$HOME/.belak/j/$dir"
			;;
		"jump")
			cd "$(readlink $HOME/.belak/j/$dir)"
			;;
	esac
}
