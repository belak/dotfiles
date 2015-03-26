# Simple platform detection
if [[ `uname` = "Darwin" ]]
then
	BELAK_OSX=true
else
	BELAK_LINUX=true
fi

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ "$SHLVL" -eq 1 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
	source "${ZDOTDIR:-$HOME}/.zprofile"
fi
