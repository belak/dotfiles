#!/bin/bash
if which vmware-user &>/dev/null
then
	vmware-user
fi
if which hostname &>/dev/null
then
	# TODO: Convert to a switch statement
	if [ `hostname` = "aahz" && -f /etc/arch-release ]
	then
		xrandr --output HDMI-0 --left-of DVI-D-0
	elif [ `hostname` = "winslow" ]
	then
		exec openbox-session
	fi
fi
exec awesome
