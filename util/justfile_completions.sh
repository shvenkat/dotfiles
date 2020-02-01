# shellcheck disable=SC2059
#
# To use these completions in bash, just source this file in your ~/.bashrc.
# To use these in zsh, just add to your `.zshrc`:
#   autoload bashcompinit
#   bashcompinit
#   source <this-file>

_just() {
    # The COMPREPLY array is used to pass the completions back to the shell.
    COMPREPLY=()
    local cmd="$1"
    local cur="${COMP_WORDS[COMP_CWORD]}"
    local prev="${COMP_WORDS[COMP_CWORD-1]}"
    local options targets

    # If just is not a word in the command so far, don't offer completions.
    # local word
    # for word in "${COMP_WORDS[@]}"; do
    #     if [[ "$word" == "just" ]]; then
    #         cmd="just"
    #         break
    #     fi
    # done
    if [[ "$cmd" != "just" ]]; then
        return 0
    fi

    # Complete short and long options.
    options="-q -v -e -l -h -V -f -d -s  --dry-run --highlight --no-highlight --quiet --clear-shell-args --verbose --dump --edit --evaluate --init --list --summary --help --version --color --justfile --set --shell --shell-arg --working-directory --completions --show"
    if [[ "$cur" == -* ]] ; then
        COMPREPLY=( $(compgen -W "$options" -- "$cur") )
        return 0
    fi

    # Complete option arguments.
    case "${prev}" in
        --color)
            COMPREPLY=($(compgen -W "auto always never" -- "$cur"))
            return 0
            ;;
        --completions)
            COMPREPLY=($(compgen -W "zsh bash fish powershell elvish" -- "$cur"))
            return 0
            ;;
        -f,--justfile)
            COMPREPLY=($(compgen -f "$cur"))
            return 0
            ;;
        --set)
            COMPREPLY=($(compgen -f "$cur"))
            return 0
            ;;
        --shell)
            COMPREPLY=($(compgen -f "$cur"))
            return 0
            ;;
        -s,--show)
            targets="$(just --summary)"
            COMPREPLY=($(compgen -W "$targets" -- "$cur"))
            return 0
            ;;
        -d,--working-directory)
            COMPREPLY=($(compgen -d "$cur"))
            return 0
            ;;
    esac

    # Complete targets.
    targets="$(just --summary)"
    COMPREPLY=($(compgen -W "$targets" -- "$cur"))
    return 0
}

complete -F _just -o bashdefault -o default just
