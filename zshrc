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

## Alias cmds
alias df="df -h"
alias du="du -h"
alias json="python -mjson.tool"

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

# Remove the extra space
#ZLE_RPROMPT_INDENT=0

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
zstyle ':vcs_info:*' stagedstr '%B%F{yellow}*%f%b'
zstyle ':vcs_info:*' unstagedstr '%B%F{red}*%f%r'

# The things we care about: type, name, folder, branch, revision, action, staged, unstaged
zstyle ':vcs_info:*' actionformats "%s:%r:%S:%b:%i:%a:%c:%u"
zstyle ':vcs_info:*' formats "%s:%r:%S:%b:%i:%a:%c:%u"

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

	# Term title
	print -Pn "\e]0;%n@%m: %~\a"

	# Read all the info from the vcs info
	IFS=: read vcs_type vcs_name vcs_folder vcs_branch vcs_rev vcs_action vcs_staged vcs_unstaged <<< "${vcs_info_msg_0_}"
	case $vcs_type in
		git)
			vcs_icon="±"
			;;
		hg)
			vcs_icon="☿"
			;;
		*)
			vcs_icon=""
			;;
	esac
	if [[ -n ${vcs_info_msg_0_} ]]
	then
		prompt_path="%F{green}$vcs_folder%f"
		vcs_string="%F{blue}$vcs_icon%f${vcs_unstaged}${vcs_staged}%F{yellow}[$vcs_name|$vcs_branch]%f"
	else
		prompt_path="%F{green}%2c%f"
		vcs_string=""
	fi
}

function ssh_prompt {
	[[ -n $SSH_CONNECTION ]] && echo -n "%F{red}[$(hostname)]%f"
}

function save_color {
	if [[ $? != 0 ]]; then
		start_color="%F{red}"
	else
		start_color="%F{yellow}"
	fi
}

function venv_prompt {
	if [[ -n $VIRTUAL_ENV ]]
	then
		echo -n "%F{red}[$(basename "$VIRTUAL_ENV")]%f "
	fi
}

function get_path {
	echo -n "${prompt_path} "
}

function get_color {
	echo -n "${start_color}"
}

case `hostname` in
	*'.mtu.edu')
		host='mtu.edu'
		;;
	*)
		host=`hostname`
		;;
esac

prompt_funcs=()
prompt_funcs+=(save_color)
prompt_funcs+=(ssh_prompt)
prompt_funcs+=(venv_prompt)
prompt_funcs+=(get_path)
prompt_funcs+=(get_color)

function run_prompt {
	for f in "${prompt_funcs[@]}"
	do
		$f
	done
}

# Set a default host char
prompt_start_char='?'

# Load any additional host related settings
if [[ -f $HOME/.belak/hosts/zsh/$host ]]; then
	source $HOME/.belak/hosts/zsh/$host
fi

PROMPT='$(run_prompt)${prompt_start_char}%f '
RPROMPT='${vcs_string}'

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
