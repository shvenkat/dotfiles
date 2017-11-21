#!/bin/sh

set -e -u -o pipefail


NAME="$(basename -- "$0")"
PREFIX="$(dirname -- "$0")"
OSTYPE="$(uname -s | tr '[:upper:]' '[:lower:]')"


# Show usage.
usage () {
    echo "Usage: $NAME [-h|--help]"
    echo "    Blah blah blah."
}


# ----------  Logging  -------------------------------------------------------

# Initialize logging by setting global variables.
if [ -t 1 ]; then
    NORMAL="\e[0m"
    BOLD="\e[1m"
    RED="\e[31m"
    YELLOW="\e[33m"
else
    NORMAL=""
    BOLD=""
    RED=""
    YELLOW=""
fi

# Output formatted error message _and exit_.
# Args:
#   $@: Messages.
error () {
    env echo -e "${RED}ERROR${NORMAL}${BOLD}   [$NAME]  $*${NORMAL}"
    exit 1
}

# Output formatted warning messages.
# Args:
#   $@: Messages.
warn () {
    env echo -e "${YELLOW}WARNING${NORMAL}${BOLD} [$NAME]  $*${NORMAL}"
}

# Output formatted informational messages.
# Args:
#   $@: Messages.
info () {
    env echo -e "${BOLD}INFO    [$NAME]  $*${NORMAL}"
}

# Run a command, logging its output. If an error is encountered, output an error message and exit.
# Args:
#   $@: Command and arguments to be run.
run () {
    info "Running: $* (in $(pwd))."
    ("$@" 2>&1 | while IFS='' read -r line; do echo "        $line"; done) \
        || error " command failed."
}


# ----------  main  ----------------------------------------------------------

# Parse arguments.
if [ $# -eq 1 ] && ([ "$1" = "-h" ] || [ "$1" = "--help" ]); then
    usage
    exit 0
fi
if [ $# -gt 0 ]; then
    usage
    exit 1
fi

# OS-specific logic.
case "$OSTYPE" in
    linux*)
        # Linux-specific logic.
        ;;
    darwin*)
        # MacOS-specific logic.
        ;;
    *)
        # Default logic.
        error "Unsupported OS: $OSTYPE"
        ;;
esac

# Common logic.
