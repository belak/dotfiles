#!/bin/bash
## Belak's setup.sh

## TODO:
#    Update OSX bootstrap
#    Fix Android SDK for OSX

## Utility functions
function pkg_installed {
	pacman -Qi $1 &> /dev/null
	return $?
}

function aur_build {
	cower -dd $1
	pushd $1
	makepkg -si --noconfirm
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
	mkdir ~/bin
	mkdir ~/code
	mkdir ~/docs
	mkdir ~/docs/downloads
	mkdir ~/pics
	mkdir ~/music
	mkdir ~/videos

	if [[ -n $BELAK_ARCH ]]
	then
		mkdir ~/docs/aur
		pushd ~/docs/aur

		pkg_list=(
			boost
			clang
			cmake
			gdb
			jdk7-openjdk
			jshon
			valgrind

			python-virtualenvwrapper
			python-pip
			python2-pip

			git
			mercurial

			alsa-utils
			aspell
			aspell-en
			firefox
			gimp
			htop
			lxappearance
			mlocate
			openssh
			rxvt-unicode
			stow
			unclutter
			weechat
			wget
			xdg-user-dirs
			zsh

			awesome
			vicious
			nitrogen
			xorg-xinit
			xorg-server
			xorg-xsetroot
			xf86-video-intel
			xf86-input-evdev
			xf86-input-synaptics
		)

		pkgs=()
		for pkg in "${pkg_list[@]}"
		do
			if ! pkg_installed $pkg
			then
				pkgs+=($pkg)
			fi
		done

		echo ${pkgs[@]}

		if [[ ${#pkgs[@]} != 0 ]]
		then
			sudo pacman -S --noconfirm "${pkgs[@]}"
		fi

		# Get cower
		# not sure why, but arch starts with curl, but no wget
		if ! pkg_installed cower
		then
			curl https://aur.archlinux.org/packages/co/cower/cower.tar.gz | tar xvz
			pushd cower
			makepkg -si --noconfirm
			popd
		fi

		## Skipped pkgs
		#lldb-svn
		#dmenu-pango

		## Aur pkgs
		aur_pkgs=(
			rcm

			dmenu-xft

			elementary-icon-theme-bzr
			gtk-theme-elementary-bzr

			sublime-text
			xlockless
		)

		# Note that we try to install all aur packages in case they've been updated
		for pkg in "${aur_pkgs[@]}"
		do
			aur_build $pkg
		done

		# build vim with some extra configuration
		if ! pkg_installed vim-hg
		then
			cower -dd vim-hg
			pushd vim-hg

			sed -i "s/depends=('gpm' 'perl' 'gawk')/depends=('gpm' 'python' 'python2')/" PKGBUILD
			sed -i 's/--with-features=big/--with-features=huge/' PKGBUILD
			sed -i 's/--with-compiledby=ArchLinux/--with-compiledby=belak/' PKGBUILD
			sed -i 's/--with-x=no/--with-x=yes/' PKGBUILD
			sed -i 's/--enable-perlinterp/--disable-perlinterp/' PKGBUILD
			sed -i 's/--disable-pythoninterp/--enable-pythoninterp/' PKGBUILD
			sed -i 's/--disable-python3interp/--enable-python3interp/' PKGBUILD
			makepkg -si --noconfirm

			popd
		fi

		# Go back to ~/.dotfiles
		popd
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
mkdir ~/.runtime
pushd ~/.runtime

# Install go
if vcs_clone hg https://code.google.com/p/go go
then
	pushd go/src
	# Make both for cross compilation
	GOOS=linux ./make.bash
	GOOS=darwin ./make.bash
	popd
fi

if [[ ! -d android-sdk ]]
then
	wget -O - http://dl.google.com/android/android-sdk_r23.0.2-linux.tgz | tar xz
	mv android-sdk-linux android-sdk
fi
popd

## Bootstrap all config files
RCRC=./rcrc rcup
git submodule update --init
vim +BundleInstall +qall
chsh -s `which zsh`
