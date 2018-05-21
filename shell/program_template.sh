#!/bin/sh

set -e -u -o pipefail


NAME="$(basename -- "$0")"
PREFIX="$(dirname -- "$0")"
OSTYPE="$(uname -s | tr '[:upper:]' '[:lower:]')"

USAGE="USAGE: $NAME [-v|--verbose] [--foo <exec>] <file>"
HELP="
SYNOPSIS

Blah, blah, blah.

$USAGE

    --foo <exec>    Use the foo at path <exec>.
    -v|--verbose    Show informational messages during execution. Repeat to
                    see debug messages too.
    -h|--help       Show this help message.

NOTE FOR NEW USERS

Yada, yada."


# ----------  functions  -----------------------------------------------------

# Format and print log messages.
#   $1: Log level: error, warn, info or debug.
#   $2 [$3 ...]: Log messages (will be concatenated with spaces in between).
log () {
    if [ -t 1 ]; then
        NORMAL="\\033[0m"
        BOLD="\\033[1m"
        RED="\\033[31m"
        YELLOW="\\033[33m"
    else
        NORMAL=""
        BOLD=""
        RED=""
        YELLOW=""
    fi
    level="$1"
    shift
    if [ "$level" = "error" ]; then
        echo -e " ${RED}E${NORMAL} ${BOLD}$*${NORMAL}" 1>&2
    elif [ "$level" = "warn" ]; then
        echo -e " ${YELLOW}W${NORMAL} ${BOLD}$*${NORMAL}" 1>&2
    elif [ "$level" = "info" ]; then
        if [ "$VERBOSITY" -gt 0 ]; then
            echo -e "   $*" 1>&2
        fi
    elif [ "$level" = "debug" ]; then
        if [ "$VERBOSITY" -gt 1 ]; then
            echo -e "   $*" 1>&2
        fi
    fi
}

# Run a command, logging its output. Errors are logged.
# Args:
#   $@: Command and arguments to be run.
run () {
    log debug "    $*"
    set +e
    output="$("$@" 2>&1)"
    retval=$?
    set -e
    echo "$output" | while IFS='' read -r line; do log debug "    $line"; done
    if [ "$retval" -ne 0 ]; then
        log error "Failed: $*"
        return 1
    fi
}

# Do some foo stuff.
# Args:
#   $1: Path to the foo executable.
#   $@: Paths to files to process.
do_stuff () {
    local foo_path="$1"
    shift
    :
}



# ----------  main  ----------------------------------------------------------

# Parse arguments.
foo_path=""
VERBOSITY=0
while [ $# -gt 0 ]; do
    case "$1" in
        --foo)
            if [ $# -lt 2 ]; then
                log error "Missing <path> argument following --foo."
                echo "$USAGE" 1>&2
                exit 1
            fi
            foo_path="$2"
            shift
            shift
            ;;
        -v|--verbose)
            VERBOSITY=$((VERBOSITY + 1))
            shift
            ;;
        -vv)
            VERBOSITY=$((VERBOSITY + 2))
            shift
            ;;
        -h|--help)
            echo "$HELP"
            exit 0
            ;;
        *)
            break
            # log error "Invalid argument $1." 1>&2
            # echo "$USAGE" 1>&2
            # exit 1
    esac
done

do_stuff "$foo_path" "$@"
