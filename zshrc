# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
	source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

unsetopt beep

# Completion stuff
setopt extendedglob

# Make it so we don't have to rehash
#setopt nohashdirs

# Prompt VCS settings
zstyle ':vcs_info:hg:*' hgrevformat '%r'
zstyle ':vcs_info:*' branchformat '%b'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' enable git hg

# Pick the char for the start of our prompt
# TODO: Use this again
case `hostname` in
	transistor)
		prompt_start_char='&'
		;;
	*)
		prompt_start_char='?'
		;;
esac

#[[ -f "$GOROOT/misc/zsh/go" ]] && source "$GOROOT/misc/zsh/go"

# Random stuff
# Disable ^s and ^q
stty -ixon

# Simple jump command
j() {
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
