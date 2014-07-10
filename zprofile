## startx on tty1
if [[ `tty` = '/dev/tty1' ]]
then
	exec startx
fi
