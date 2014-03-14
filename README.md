# Dotfiles

## Setup

This repo uses GNU Stow. In order to enable a module, make sure your cwd is the dotfiles repo. Then simply run stow module. That will symlink everything needed from the module into .., usually $HOME.

Note that the panel folder is not a module

## Extras

### panel

Dependencies:
```
Go (for building)
xft
dzen2-git
sutils-git
xtitle-git
```


## Modules

### vcs

This contains my personal info for the .gitconfig and .hgrc. This should probably not be used for other dotfiles repos, though the other settings are good.

### vim

This contains the .vimrc as well as some stuff in the .vim folder. Be sure the vundle repo is checked out before running vim (usually git submodule update --init)

### xorg

This contains a basic .xinitrc, .Xdefaults as well as my theme file in .belak/xcolors. This also includes a config for the awesome wm.

### zsh

This contains my .zshrc as well as a few custom tweaks. The gentoo prompt is included from their repo so it can be used on systems other than gentoo.
