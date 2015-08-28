# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# Load some things
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
[ -f ~/.rbenv ] && eval "$(rbenv init -)"

# Env vars
GOPATH=$HOME/go
