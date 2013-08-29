#!/bin/bash
## Belak's setup.sh

## NOTE
# Even though this sets up config files, .gitconfig and .hgrc are in setup.sh

## TODO
# Make all user input only needed at the start
# MPD autostart on OSX

## Useful functions
function config_link {
	if [[ ! -e ~/.$1 ]]
	then
		ln -s `pwd`/$1 ~/.$1
	fi
}

## Platform Detection
if [[ `uname` = "Darwin" ]]
then
	BELAK_OSX=true
else
	BELAK_LINUX=true
	if [[ -f /etc/debian_version ]]
	then
		if [[ -f /etc/lsb_release ]]
		then
			BELAK_UBUNTU=true
		else
			BELAK_DEBIAN=true
		fi
	elif [[ -f /etc/arch-release ]]
	then
		BELAK_ARCH=true
	fi
fi

## Setup the dotfiles
git submodule update --init &>/dev/null

config_link vimrc
config_link vim
config_link zshrc
config_link Xdefaults
config_link belak

vim +BundleInstall +qall
