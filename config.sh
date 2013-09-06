#!/bin/bash
## Belak's config.sh

<<<<<<< HEAD
if [[ ! -f "$HOME/.ssh/id_rsa.pub" ]]
then

	read -p "Bitbucket Username: " bb_user
	read -s -p "Bitbucket Password: " bb_pass
	echo

	read -p "Github Username: " gh_user
	read -s -p "Github Password: " gh_pass
	echo

	ssh-keygen
	curl -u "$bb_user:$bb_pass" --data-urlencode "key=$(<$HOME/.ssh/id_rsa.pub)" --request POST https://api.bitbucket.org/1.0/ssh-keys/

	echo "{}" | jshon -s "$(hostname)" -i "title" -s "$(<$HOME/.ssh/id_rsa.pub)" -i "key" | curl -u "$gh_user:$gh_pass" --data @- https://api.github.com/user/keys
fi

git submodule update --init &>/dev/null

stow vcs
stow vim
stow xorg
stow zsh

vim +BundleInstall +qall
