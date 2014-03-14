#!/bin/bash
if which vmware-user &>/dev/null
then
	vmware-user
fi

xsetroot -cursor_name left_ptr &
unclutter -grab -root &

panel &

sxhkd &
exec bspwm
