#!/bin/bash
xrdb -I"$HOME" "$HOME/.Xresources"

xsetroot -cursor_name left_ptr &

unclutter -grab -root &

#nitrogen --set-auto "$(find $HOME/pics/Wallpapers | sort -R | head -1)"
nitrogen --restore

sxhkd &
exec bspwm
