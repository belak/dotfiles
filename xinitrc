#!/bin/bash

# This is a workaround for emacs because we need this var set before
# we even get to this point.
export ALTERNATE_EDITOR=""

if which vmware-user-suid-wrapper &>/dev/null; then
	vmware-user-suid-wrapper
fi

xrdb -I"$HOME" "$HOME/.Xresources"
xsetroot -cursor_name left_ptr &
unclutter &

[[ -f "$HOME/.xinitrc-local" ]] && . "$HOME/.xinitrc-local"

exec i3
