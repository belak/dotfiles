#!/bin/bash
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

mkdir ~/code
mkdir ~/docs
mkdir ~/pics
mkdir ~/music
mkdir ~/videos

# Install packages - important to do this before we need them
if [[ -e /etc/arch-release && ! -f .arch-init ]]
then
	mkdir ~/docs/aur
	su -c "pacman -S sudo"
	su -c visudo
	sudo pacman -S htop zsh git mercurial base-devel boost xdg-user-dirs alsa-utils \
		cmake faience-icon-theme lxappearance mlocate openssh \
		python-virtualenvwrapper python-pip python2-pip \
		rxvt-unicode xorg-server xorg-xinit xorg-xrandr wget terminus-font

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

git submodule update --init
if [ ! -f ~/.gitconfig ]
then
        git config --global user.email kelwert@mtu.edu
        git config --global user.name "Kaleb Elwert"
        git config --global push.default simple
fi

config_link vimrc
config_link vim
config_link zshrc
config_link Xdefaults
config_link belak
config_link hgrc

# Setup the runtime folder
if [[ ! -d ~/.runtime ]]
then
	# I really just wanted to use this
	pushd .
	
	mkdir ~/.runtime
	cd ~/.runtime

	# Install go
	pushd .
	hg clone https://code.google.com/p/go
	cd go/src
	./make.bash
	popd

	# Cocos2d-X
	git clone https://github.com/cocos2d/cocos2d-x

	# go back to where the dotfiles are
	popd
fi

vim +BundleInstall +qall
