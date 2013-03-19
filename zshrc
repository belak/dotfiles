## Env vars
if [[ -z $DISPLAY ]]
then
	export EDITOR="vim"
	export GOROOT="$HOME/.runtime/go"
	export GOPATH="$HOME/go"
	export WORKON_HOME="$HOME/.runtime/python"
	export PATH="$HOME/.rbenv/bin:$GOROOT/bin:$GOPATH/bin:$HOME/bin:$PATH"
fi
if which rbenv &>/dev/null
then
	eval $(rbenv init -)
fi

if which virtualenvwrapper.sh &>/dev/null
then
	source $(which virtualenvwrapper.sh)
elif [[ -f /etc/bash_completion.d/virtualenvwrapper ]]
then
	source /etc/bash_completion.d/virtualenvwrapper
fi	

## startx on tty1
if [[ `tty` = '/dev/tty1' ]]
then
        exec startx
fi

alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias df="df -h"
alias du="du -h"

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
setopt prompt_subst

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
autoload -Uz compinit promptinit vcs_info colors
compinit
promptinit
colors

## Prompt stuff
prompt gentoo
zstyle ':vcs_info:hg:*' hgrevformat '%r'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' enable git hg svn
zstyle ':vcs_info:*' stagedstr '*'
zstyle ':vcs_info:*' unstagedstr '*'
zstyle ':vcs_info:*' branchformat '%b|%r'
zstyle ':vcs_info:*' actionformats " %{$fg_bold[yellow]%}%c%{$fg_bold[red]%}%u%{$fg_bold[white]%}[%{$fg_bold[yellow]%}%s|%b|%a%{$fg_bold[white]%}]%{$reset_color%}"
zstyle ':vcs_info:*' formats " %{$fg_bold[yellow]%}%c%{$fg_bold[red]%}%u%{$fg_bold[white]%}[%{$fg_bold[yellow]%}%s|%b%{$fg_bold[white]%}]%{$reset_color%}"
function precmd {
        vcs_info
}
function ssh_prompt {
        [[ -n $SSH_CONNECTION ]] && echo "%{$fg_bold[red]%}(ssh) %{$reset_color%}"
}
PROMPT='$(ssh_prompt)'"$PROMPT"
RPROMPT='${vcs_info_msg_0_}'

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

# setup key accordingly
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
