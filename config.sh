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
		git config --global branch.autorebase always
	fi
fi
