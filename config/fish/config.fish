if status is-interactive
    # Commands to run in interactive sessions can go here
end

#
# Utils
#

if type -q direnv
    direnv hook fish | source
end

if type -q jump
    jump shell fish | source
end

if type -q fzf
    fzf --fish | source
end

if type -q pyenv
    set -gx PYENV_ROOT $HOME/.pyenv
    fish_add_path $PYENV_ROOT/bin

    pyenv init - | source

    if type -q pyenv-virtualenv-init
        pyenv virtualenv-init - | source
    end
end

if type -q rbenv
    rbenv init - --no-rehash fish | source
end

if type -q volta
    set -gx VOLTA_HOME $HOME/.volta
    fish_add_path $VOLTA_HOME/bin
end

# yarn-switch isn't in nixpkgs but is required for work, so if it's
# installed, we load it.
if test -f $HOME/.yarn/switch/env.fish
    source $HOME/.yarn/switch/env.fish
end

#
# Settings
#

set -g fish_autosuggestion_enabled 0
set fish_greeting

#
# Local settings
#

if test -f ~/.config/fish/config.local.fish
    source ~/.config/fish/config.local.fish
end
