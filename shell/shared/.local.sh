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
