# Environment variables

# Go
set -gx GOPATH $HOME/go

# Volta (Node version manager)
set -gx VOLTA_HOME $HOME/.volta

# Default applications
set -gx ALTERNATE_EDITOR ""
set -gx EDITOR nvim
set -gx VISUAL nvim
set -gx PAGER less

# Less pager options
set -gx LESS '-F -g -i -M -R -S -w -z-4'

# Python
set -gx WORKON_HOME $HOME/.virtualenvs
set -gx PYTHONDONTWRITEBYTECODE true
set -gx VIRTUAL_ENV_DISABLE_PROMPT true

# FZF - prefer using fd for file finding
if type -q fd
    set -gx FZF_DEFAULT_COMMAND 'fd --type file'
end
