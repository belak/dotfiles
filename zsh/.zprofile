## startx on tty1
if [[ `tty` = '/dev/tty1' ]]
then
	env > /tmp/belak-env
	exec startx
fi
