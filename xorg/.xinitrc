#!/bin/bash
if which hostname &>/dev/null
then
	if [[ `hostname` == "aahz" && -f /etc/arch-release ]]
	then
		xrandr --output HDMI-0 --left-of DVI-D-0
	fi
fi
exec awesome
