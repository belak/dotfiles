#!/bin/bash
xsetroot -cursor_name left_ptr &

if [[ `uname` = "Darwin" ]]
then
	cd $HOME
	source $HOME/.profile
	export LANG="en_US.UTF-8"
	exec >>~/.xsession-errors 2>&1
	sxhkd -c $HOME/.config/sxhkd/osx-sxhkdrc &
	exec bspwm -c $HOME/.config/bspwm/bspwmrc
else
	if which vmware-user &>/dev/null
	then
		vmware-user
	fi

	unclutter -grab -root &

	#nitrogen --set-auto "$(find $HOME/pics/Wallpapers | sort -R | head -1)"
	nitrogen --restore

	exec awesome
fi
