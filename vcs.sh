#!/bin/bash

read -p "User: " vcs_user
read -p "Email: " vcs_email

read -p "Bitbucket Username: " bb_user
read -s -p "Bitbucket Password: " bb_pass
bb_auth="$bb_user:$bb_pass"

read -p "Github Username: " gh_user
read -s -p "Github Password: " gh_pass
gh_auth="$gh_user:$gh_pass"

[[ ! -f "$HOME/.ssh/id_rsa.pub" ]] && ssh-keygen

curl -u "$bb_auth" --data-urlencode "key=$(<$HOME/.ssh/id_rsa.pub)" --request POST https://api.bitbucket.org/1.0/ssh-keys/
echo "{}" | jshon -s "$(hostname)" -i "title" -s "$(<$HOME/.ssh/id_rsa.pub)" -i "key" | curl -u "$gh_auth" --data @- https://api.github.com/user/keys

git config --global user.name "$vcs_user"
git config --global user.email "$vcs_email"

[[ ! -f "$HOME/.hgrc" ]] && printf '[ui]\nusername = %s <%s>\n' "$vcs_user" "$vcs_email" > $HOME/.hgrc
