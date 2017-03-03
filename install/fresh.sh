#!/bin/sh

set -e -u -o pipefail

DOTFILES_SRC="https://github.com/shvenkat/dotfiles"
DOTFILES_CLONE="${HOME}/.fresh/source/shvenkat/dotfiles"
FRESH_SRC="https://github.com/freshshell/fresh"
FRESH_CLONE="${HOME}/.fresh/source/freshshell/fresh"
FRESHRC_SRC="${DOTFILES_CLONE}/install/freshrc"
FRESHRC_DST="${HOME}/.freshrc"
NAME="$(basename "$0")"


# Initialize logging by setting global variables.
init_logger () {
    if [ -t 1 ]; then
        NORMAL="\e[0m"
        RED="\e[31m"
        YELLOW="\e[33m"
    else
        NORMAL=""
        RED=""
        YELLOW=""
    fi
}


# Output formatted error message and exit.
error () {
    env echo -e "${RED}ERROR${NORMAL}  [$NAME]  $*."
    exit 1
}


# Output formatted warning messages.
warn () {
    env echo -e "${YELLOW}WARN${NORMAL}   [$NAME]  $*."
}


# Output formatted informational messages.
info () {
    env echo -e "INFO   [$NAME]  $*."
}


# Run a command, logging its output. If an error is encountered, output an error message and exit.
# Args:
#   $@: Command and arguments to be run.
run () {
    info "Running command $* (in $(pwd))."
    ("$@" 2>&1 | while IFS='' read -r line; do echo "        $line"; done) \
        || error "$1 command failed."
}


# Clone a git repo, or update (pull) an existing one. Fail if the destination exists and cannot be
# updated to match the result of a clone operation.
# Args:
#   $1: Remote URL, e.g. https://github.com/foo/bar.
#   $2: Local path.
clone_safely () {
    if [ ! -e "$2" ]; then
        run git clone "$1" "$2"
    else
        # Attempt to replicate the effect of 'git clone' in a non-destructive way. To do this,
        # verify or satisfy the following assertions and fail otherwise.
        # * The local path is a git repo, in a clean state with no untracked files.
        # * The git repo has a remote named 'origin' matching the given URL.
        # * The git repo has a branch that tracks the origin branch referred to by origin/HEAD.
        # Checkout and update (fetch and merge) the appropriate local branch.

        # Verify that the local path is a directory.
        test -d "$2" \
            || error "Cannot clone $1; incompatible file type (non-directory) exists at $2."
        current_dir="$(pwd)"
        cd "$2"
        # Verify that the local path is a git repository.
        git rev-parse --is-inside-work-tree >/dev/null 2>&1 \
            || error "Cannot clone $1 to $2; existing directory is not a git repository."
        # Verify that the working tree is clean regarding tracked files.
        git diff-index --quiet HEAD >/dev/null 2>&1 \
            || error "Cannot clone $1 to $2; existing git repo is not clean."
        # Verify that there are no untracked files. These could conflict with a pull.
        test -z "$(git ls-files --others --exclude-standard 2>/dev/null)" \
            || error "Cannot clone $1 to $2; existing git repo has untracked files."
        # Verify that a remote named 'origin' exists and has the given URL.
        if (git remote | grep -q '^origin$') >/dev/null 2>&1; then
            # Verify that the URL of the remote 'origin' matches the given one.
            test "$1" = "$(git ls-remote --get-url origin 2>/dev/null)" \
                || error "Cannot clone $1 to $2; existing git repo has a different 'origin' remote."
        else
            # Add an 'origin' remote.
            run git remote add origin "$1"
        fi
        # Determine the default remote branch i.e. resolve origin/HEAD. Typically 'master'.
        origin_head="$(git ls-remote --symref origin HEAD \
                        | grep -o 'refs/heads/[^[:space:]]\+')" 2>/dev/null \
            || error "Cannot clone $1 to $2; unable to resolve origin/HEAD for existing git repo."
        origin_head="${origin_head#refs/heads/}"
        # Ensure that a local tracking branch exists for the default remote branch. Identify such a
        # local branch or create one.
        tracking_heads="$(git for-each-ref --format='%(refname) %(upstream)' refs/heads \
                            | (grep " refs/remotes/origin/$origin_head" || true) \
                            | cut -d' ' -f1 \
                            | sed -e 's#refs/heads/##')" 2>/dev/null
        if [ -z "$tracking_heads" ]; then
            # Verify the absence of a local branch with the same name as origin/HEAD.
            git show-ref --verify --quiet "refs/heads/$origin_head" 2>/dev/null \
                && error "Cannot clone $1 to $2; existing git repo has a $origin_head branch " \
                    "that does not track origin/$origin_head (the current origin/HEAD)."
            # Create a local branch that tracks origin/HEAD.
            run git checkout -b "$origin_head" --track "origin/$origin_head"
        else
            # If there is a single matching local branch, use it. If there are multiple, use the one
            # with the same name as the remote branch. Otherwise, select one arbitrarily.
            if echo "$tracking_heads" | grep -q "^${origin_head}$"; then
                local_head="${origin_head}"
            else
                local_head="$(echo "$tracking_heads" | head -n1)" 2>/dev/null
            fi
            run git checkout "$local_head"
        fi
        # Fetch and merge.
        run git pull origin "$origin_head"
        cd "$current_dir"
    fi
}


# Create a symlink. Fail if the target exists and is not a symlink.
# Args:
#   $1: Source path.
#   $2: Destination path.
symlink_safely () {
    # If a non-symlink exists, abort with a warning.
    if [ -e "$FRESHRC_DST" ] && [ ! -h "$FRESHRC_DST" ]; then
        echo "Warning: $FRESHRC_DST exists, aborting."
        exit 1
    fi
    run ln -sf "$1" "$2"
}


# ----------  main  ----------------------------------------------------------

# Install fresh config.
clone_safely "$DOTFILES_SRC" "$DOTFILES_CLONE"
symlink_safely "$FRESHRC_SRC" "$FRESHRC_DST"

# Run fresh.
clone_safely "$FRESH_SRC" "$FRESH_CLONE"
"$FRESH_CLONE/bin/fresh"
