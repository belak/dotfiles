function fish_prompt
    set -l last_status $status

    # Conditional prompt data
    set -l prompt_status_color
    set -l prompt_prefix
    set -l prompt_virtualenv

    # If the last command failed, set the final prompt segment to red
    if test $last_status -ne 0
        set prompt_status_color (set_color red)
    else
        set prompt_status_color (set_color brblue)
    end

    # If we're connected via SSH, display the username/hostname
    if test -n "$SSH_TTY"
        set prompt_prefix (whoami)@(prompt_hostname)' '
    end

    if test -n "$VIRTUAL_ENV"
        set prompt_virtualenv (set_color normal)" ["(basename $VIRTUAL_ENV)"]"
    end

    set -l prompt_char (if test (id -u) -eq 0; echo -n ' # '; else echo -n ' > '; end)

    string join '' -- \
        $prompt_prefix \
        (set_color brgreen)(prompt_pwd) \
        $prompt_virtualenv \
        $prompt_status_color$prompt_char \
        (set_color normal)
end

function fish_right_prompt
    # Separate the commit from the state with a space rather than a pipe
    set -fx __fish_git_prompt_char_stateseparator " "

    # Various tweaks to make the display more informative
    set -fx __fish_git_prompt_showcolorhints true
    set -fx __fish_git_prompt_showdirtystate true
    set -fx __fish_git_prompt_showupstream informative
    set -fx __fish_git_prompt_showdirtystate true
    set -fx __fish_git_prompt_showuntrackedfiles true

    # Settings for the branch name
    set -fx __fish_git_prompt_color_branch normal

    # Settings for the current state
    #
    # Note: I'm a bit picky when it comes to prompts, so this replaces all the
    # unicode characters with an ascii alternative. We currently use the same
    # character for everything and rely on the color to differentiate.
    set -fx __fish_git_prompt_char_stagedstate '*'
    set -fx __fish_git_prompt_color_stagedstate green
    set -fx __fish_git_prompt_char_dirtystate '*'
    set -fx __fish_git_prompt_color_dirtystate yellow
    set -fx __fish_git_prompt_char_invalidstate '*'
    set -fx __fish_git_prompt_color_invalidstate magenta
    set -fx __fish_git_prompt_char_untrackedfiles '*'
    set -fx __fish_git_prompt_color_untrackedfiles red

    # Settings for ahead/behind information
    #
    # Note: there are no color variables for "upstream" data, but we can throw
    # it in with the ahead/behind characters and it works just the same.
    set -fx __fish_git_prompt_char_upstream_prefix ' '
    set -fx __fish_git_prompt_char_upstream_ahead (set_color green)'+'
    set -fx __fish_git_prompt_char_upstream_behind (set_color red)'-'

    string join '' -- \
        (fish_git_prompt "%s") \
        (set_color normal)
end
