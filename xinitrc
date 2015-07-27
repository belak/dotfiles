#!/bin/bash

# This is a workaround for emacs because we need this var set before
# we even get to this point.
export ALTERNATE_EDITOR=""

if which vmware-suid-user-wrapper &>/dev/null; then
	vmware-suid-user-wrapper
fi

xrdb -I"$HOME" "$HOME/.Xresources"
xsetroot -cursor_name left_ptr &
unclutter -grab -root &

#nitrogen --set-auto "$(find $HOME/pics/Wallpapers | sort -R | head -1)"

exec i3
