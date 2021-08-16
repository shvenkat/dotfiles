# ----------  Secrets  -------------------------------------------------------


# ----------  Path  ----------------------------------------------------------

__local_bin="..."
if [[ -d "$__local_bin" ]] && [[ ! "$PATH" =~ "$__local_bin" ]]; then
    export PATH="${PATH}:${__local_bin}"
fi
unset __local_bin

if [[ -n "$PS1" ]]; then
    CDPATH=".:...:${HOME}"
fi


# ----------  Aliases  -------------------------------------------------------

alias diff_something="rsync -n -aOzi --delete \
    --exclude ... --include '/foo/*.bar' --exclude /foo/ \
    /local/path/ machine:/remote/path/"


# ----------  Completion  ----------------------------------------------------

if [[ -n "$ZSH_VERSION" ]]; then
    autoload bashcompinit
    bashcompinit
fi


# ----------  Python virtualenv  ---------------------------------------------

if [[ -n "$BASH_VERSION" && -n "$PS1" && "$TERM" != 'dumb' ]] && tty -s; then
    # Updates $PS1.
    __update_prompt () {
        if [[ -n "$VIRTUAL_ENV" ]]; then
            prompt_color="\[\033[91m\]"
        else
            prompt_color="\[\033[30m\]"
        fi
        PS1="${HOSTNAME:0:4} $(__shrink_path "$(pwd)") ${prompt_color}◀▶\[\033[0m\] "
        unset prompt_color
    }
    # Set the prompt now.
    __update_prompt
fi
