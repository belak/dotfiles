#!/bin/bash
## Belak's setup.sh

## TODO
# Make all user input only needed at the start
# MPD autostart on OSX

function aur_build {
	pushd .
	cower -dd $1
	cd $1
	makepkg -si
	popd
}

function vcs_clone {
	if [[ ! -d "$3" ]]
	then
		mkdir -p "$(dirname $3)" &>/dev/null
		$1 clone "$2" "$3"
	
		return 0
	fi

	return 1
}

function brew_install {
	brew install "$@" &>/dev/null
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

## Platform specific setup
if [[ -n $BELAK_LINUX ]]
then
	if ! which sudo &>/dev/null
	then
		echo "Couldn't find sudo"
		exit 1
	fi

	# Make our directories
	mkdir ~/code
	mkdir ~/docs
	mkdir ~/pics
	mkdir ~/music
	mkdir ~/videos

	if [[ -n $BELAK_ARCH && ! -f .arch-init ]]
	then
		mkdir ~/docs/aur
		sudo pacman -S stow awesome vicious htop zsh git mercurial base-devel boost xdg-user-dirs \
			alsa-utils cmake faience-icon-theme lxappearance mlocate openssh \
			python-virtualenvwrapper python-pip python2-pip \
			rxvt-unicode xorg-server xorg-xinit xorg-xrandr wget terminus-font jshon \
			xf86-video-intel xf86-input-evdev xf86-input-synaptics

		pushd .

		# get cower
		cd ~/docs/aur
		# not sure why, but arch starts with curl, but no wget
		curl https://aur.archlinux.org/packages/co/cower/cower.tar.gz | tar xvz
		pushd .
		cd cower
		makepkg -si
		popd

		# build the easy ones
		aur_build wmname-git
		aur_build google-chrome
		aur_build zukitwo-themes
		aur_build sublime-text
		aur_build dmenu-pango

		# build vim with some extra configuration
		pushd .
		cower -dd vim-hg
		cd vim-hg
		sed -i "s/depends=('gpm' 'perl' 'python' 'python2')/depends=('gpm' 'ruby')/" PKGBUILD
		sed -i 's/--with-compiledby=ArchLinux/--with-compiledby=belak/' PKGBUILD
		sed -i 's/--with-x=no/--with-x=yes/' PKGBUILD
		sed -i 's/--enable-perlinterp/--disable-perlinterp/' PKGBUILD
		sed -i 's/--disable-rubyinterp/--enable-rubyinterp/' PKGBUILD
		makepkg -si
		popd

		# Go back to ~/.dotfiles
		popd

		touch .arch-init
	fi

	# Set user dirs
	xdg-user-dirs-update --set DOWNLOAD ~/docs/downloads
	xdg-user-dirs-update --set DOCUMENTS ~/docs
	xdg-user-dirs-update --set MUSIC ~/music
	xdg-user-dirs-update --set PICTURES ~/pics
	xdg-user-dirs-update --set VIDEOS ~/videos

	# Unset these
	xdg-user-dirs-update --set TEMPLATES ~
	xdg-user-dirs-update --set DESKTOP ~
	xdg-user-dirs-update --set PUBLICSHARE ~
fi

if [[ -n $BELAK_OSX ]]
then
	# Install brew
	if ! which brew &>/dev/null
	then
		ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"

		# Essentials
		brew_install bash coreutils findutils zsh

		# Tools
		brew_install cloc git lftp mercurial ntfs-3g tmux vim wget

		# Languages
		brew_install lua52

		# Misc
		brew_install jshon mpd mpc ncmpcpp pianobar
	fi
fi

## Setup the runtime folder
#if [[ ! -d ~/.runtime ]]
#then
	pushd .

	mkdir ~/.runtime
	cd ~/.runtime

	# Install go
	pushd .
	if vcs_clone hg https://code.google.com/p/go go
	then
		cd go/src
		# Make both for cross compilation
		GOOS=linux ./make.bash
		GOOS=darwin ./make.bash
	fi
	popd

	# Cocos2d-X
	read -p "Install Cocos2d-X? [y/N] " install_cocos
	if [[ $install_cocos = "y" || $install_cocos = "Y" ]]
	then
		vcs_clone git https://github.com/cocos2d/cocos2d-x cocos2d-x
	fi

	# go back to where the dotfiles are
	popd
#fi
