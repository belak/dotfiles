#!/bin/bash
if which vmware-user &>/dev/null
then
	vmware-user
fi

xsetroot -cursor_name left_ptr &
unclutter -grab -root &

#nitrogen --set-auto "$(find $HOME/pics/Wallpapers | sort -R | head -1)"
nitrogen --restore

exec i3
