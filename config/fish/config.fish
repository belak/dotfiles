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

#
# Settings
#

set -g fish_autosuggestion_enabled 0
set fish_greeting

