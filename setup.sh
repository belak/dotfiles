#!/bin/bash
## Belak's setup.sh

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

function aur_build {
	pushd .
	cower -dd $1
	cd $1
	makepkg -si
	popd
}

function brew_install {
	brew install "$@" &>/dev/null
}

function cask_install {
	brew cask install "$@" &> /dev/null
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
config_link hgrc

vim +BundleInstall +qall

## Platform specific setup
if [[ -n $BELAK_LINUX ]]
then
	# Make our directories
	mkdir ~/code
	mkdir ~/docs
	mkdir ~/pics
	mkdir ~/music
	mkdir ~/videos

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

	if [[ -n $BELAK_ARCH && ! -f .arch-init ]]
	then
		mkdir ~/docs/aur
		su -c "pacman -S sudo"
		su -c visudo
		sudo pacman -S htop zsh git mercurial base-devel boost xdg-user-dirs alsa-utils \
			cmake faience-icon-theme lxappearance mlocate openssh \
			python-virtualenvwrapper python-pip python2-pip \
			rxvt-unicode xorg-server xorg-xinit xorg-xrandr wget terminus-font jshon

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

		pushd .
		cower -dd dwm-pango
		cd dwm-pango
		# My custom patches
		sed -i 's/pango_layout_set_text/pango_layout_set_markup/' dwm-6.0-pango.patch
		sed -i 's/y = dc.y;/y = dc.y + 1;/' dwm-6.0-pango.patch
		cp ~/.dotfiles/dwm-config.h config.h
		makepkg -si --skipinteg
		popd

		# Go back to ~/.dotfiles
		popd

		touch .arch-init
	fi
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

[[ ! -f "$HOME/.ssh/id_rsa.pub" ]] && ssh-keygen

if [[ ! -f ~/.hgrc || ! -f ~/.gitconfig ]]
then
	## VCS setup
	read -p "VCS User: " vcs_user
	read -p "VCS Email: " vcs_email

	if [[ ! -f ~/.hgrc ]]
	then
		read -p "Bitbucket Username: " bb_user
		read -s -p "Bitbucket Password: " bb_pass
		curl -u "$bb_user:$bb_pass" --data-urlencode "key=$(<$HOME/.ssh/id_rsa.pub)" --request POST https://api.bitbucket.org/1.0/ssh-keys/
		[[ ! -f "$HOME/.hgrc" ]] && printf '[ui]\nusername = %s <%s>\n' "$vcs_user" "$vcs_email" > $HOME/.hgrc
	fi

	if [[ ! -f ~/.gitconfig ]]
	then
		read -p "Github Username: " gh_user
		read -s -p "Github Password: " gh_pass
		echo "{}" | jshon -s "$(hostname)" -i "title" -s "$(<$HOME/.ssh/id_rsa.pub)" -i "key" | curl -u "$gh_user:$gh_pass" --data @- https://api.github.com/user/keys
		git config --global user.name "$vcs_user"
		git config --global user.email "$vcs_email"
		git config --global push.default simple
	fi
fi

## Setup the runtime folder
if [[ ! -d ~/.runtime ]]
then
	pushd .

	mkdir ~/.runtime
	cd ~/.runtime

	# Install go
	pushd .
	hg clone https://code.google.com/p/go
	cd go/src
	# Make both for cross compilation
	GOOS=linux ./make.bash
	GOOS=darwin ./make.bash
	popd

	# Cocos2d-X
	read -p "Install Cocos2d-X? [y/N] " install_cocos
	if [[ $install_cocos = "y" || $install_cocos = "Y" ]]
	then
		git clone https://github.com/cocos2d/cocos2d-x
	fi

	# go back to where the dotfiles are
	popd
fi
