# ----------  Secrets  -------------------------------------------------------


# ----------  Path  ----------------------------------------------------------

__local_bin="..."
if [[ -d "$__local_bin" ]]; then
    if [[ ! "$PATH" =~ "$__local_bin" ]]; then
        export PATH="${PATH}:${__local_bin}"
    fi
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
        PS1="${HOSTNAME:0:4} $(__shrink_path "$(pwd)") ${VIRTUAL_ENV:+V }◀▶ "
    }
    # Do this before leaving the old directory.
    __pre_leave () {
        if [[ -n "$VIRTUAL_ENV" ]]; then
            deactivate
        fi
    }
    # Do this after entering the new directory.
    __post_enter () {
        if [[ "$PWD" =~ ^/trusted/path/ && -r "$PWD/.venv/bin/activate" ]]; then
            source "$PWD/.venv/bin/activate"
        fi
        __update_prompt
    }
    # Redefine cd, pushd and popd to update the prompt.
    cd () {
        __pre_leave && builtin cd "$@" && __post_enter
    }
    pushd () {
        __pre_leave && builtin pushd "$@" && __post_enter
    }
    popd () {
        __pre_leave && builtin popd && __post_enter
    }
    # Set the prompt now.
    __update_prompt
fi
