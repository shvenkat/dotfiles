# shellcheck disable=SC2059
#
# This file provides completion of just options and justfile targets. The simple
# use case is assumed i.e. a justfile is present in the current directory and
# just is being run with it. In other words, -d,--working-directory and
# --justfile are not used.
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

    # If the command is not just, don't offer completions.
    if [[ "$cmd" != "just" ]]; then
        return 0
    fi

    # Complete short and long options.
    options="
            --clear-shell-args
            --color
            --completions
            --dry-run
            --dump
        -e  --edit
            --evaluate
        -f  --justfile
        -h  --help
            --highlight
            --init
        -l  --list
            --no-highlight
        -q  --quiet
            --set
            --shell
            --shell-arg
        -s  --show
            --summary
        -v  --verbose
        -V  --version
        -d  --working-directory
    "
    if [[ "$cur" == -* ]] ; then
        # COMPREPLY=( $(compgen -W "$options" -- "$cur") )
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
    # targets="$(just --summary)"
    # COMPREPLY=($(compgen -W "$targets" -- "$cur"))
    COMPREPLY=($(just --summary | tr ' ' '\n' | fzf --filter "$cur" | tr '\n' ' '))
    # COMPREPLY=("pull-images" "rebuild-images")
    # echo "${COMPREPLY[@]}"
    return 0
}

complete -F _just -o bashdefault -o default just
