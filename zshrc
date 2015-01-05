# Belak's zshrc

if which virtualenvwrapper.sh &>/dev/null
then
	source $(which virtualenvwrapper.sh)
elif [[ -f /etc/bash_completion.d/virtualenvwrapper ]]
then
	source /etc/bash_completion.d/virtualenvwrapper
elif [[ -f /usr/share/virtualenvwrapper/virtualenvwrapper.sh ]]
then
	source /usr/share/virtualenvwrapper/virtualenvwrapper.sh
fi

## Magical stuff
. $HOME/.dotfiles/base16-shell/base16-tomorrow.dark.sh

## Alias cmds
alias df="df -h"
alias du="du -h"
alias json="python -mjson.tool"

if [[ -n $BELAK_LINUX ]]; then
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
if [[ -f /etc/gentoo-release ]]; then
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
zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:*' stagedstr '*'
zstyle ':vcs_info:*' unstagedstr '*'
zstyle ':vcs_info:*' branchformat '%b|%r'
zstyle ':vcs_info:*' actionformats "%F{red}%u%F{yellow}%c %F{red}%a %F{green}%S%f"
zstyle ':vcs_info:*' formats "%F{red}%u%F{yellow}%c %F{green}%S%f"

start_time=$SECONDS
cmd=
function preexec {
	start_time=$SECONDS
	cmd=$1
}

function precmd {
	# Show a timer if we took longer than 10 seconds
	if [[ -n $cmd ]]
	then
		timer_result=$(($SECONDS-$start_time))
		h=$(($timer_result/3600))
		m=$((($timer_result/60)%60))
		s=$(($timer_result%60))

		if [[ $h -gt 0 ]]
		then
			print -P "%B%F{red}>>> elapsed time ${h}h${m}m${s}s%b\n"
		elif [[ $m -gt 0 ]]
		then
			print -P "%B%F{yellow}>>> elapsed time ${m}m${s}s%b\n"
		elif [[ $s -gt 10 ]]
		then
			print -P "%B%F{green}>>> elapsed time ${s}s%b\n"
		fi
	fi
	start_time=$SECONDS
	cmd=

	vcs_info
	print -Pn "\e]0;%n@%m: %~\a"
	if [[ -n ${vcs_info_msg_0_} ]]; then
		prompt_path=${vcs_info_msg_0_}
	else
		prompt_path="%F{green}%2c%f"
	fi
}

function ssh_prompt {
	[[ -n $SSH_CONNECTION ]] && echo "↕ "
}

function prompt_start_color {
	if [[ $? != 0 ]]; then
		echo "%F{red}"
	else
		echo "%F{yellow}"
	fi
}

case `hostname` in
	*'.mtu.edu')
		host='mtu.edu'
		;;
	*)
		host=`hostname`
		;;
esac

# Set a default host char
prompt_start_char='?'

# Load any additional host related settings
if [[ -f $HOME/.belak/hosts/zsh/$host ]]; then
	source $HOME/.belak/hosts/zsh/$host
fi

if [[ -n ${prompt_start_char} ]]
then
	prompt_start_char+=' '
fi

PROMPT='$(prompt_start_color)$(ssh_prompt)${prompt_start_char}%f'
RPROMPT='${prompt_path}%f'

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
