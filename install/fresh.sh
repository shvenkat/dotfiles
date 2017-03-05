#!/bin/sh

set -e -u -o pipefail


DOTFILES_URL="https://github.com/shvenkat/dotfiles"
DOTFILES_CLONE="${HOME}/.fresh/source/shvenkat/dotfiles"
FRESH_URL="https://github.com/freshshell/fresh"
FRESH_CLONE="${HOME}/.fresh/source/freshshell/fresh"
FRESHRC_TARGET="${DOTFILES_CLONE}/install/freshrc"
FRESHRC_SYMLINK="${HOME}/.freshrc"
NAME="$(basename "$0")"


# ----------  functions  -----------------------------------------------------

# Initialize logging by setting global variables.
init_logger () {
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
}


# Output formatted error message and exit.
error () {
    env echo -e "${RED}ERROR${NORMAL}${BOLD}  [$NAME]  $*${NORMAL}"
    exit 1
}


# Output formatted warning messages.
warn () {
    env echo -e "${YELLOW}WARN${NORMAL}${BOLD}   [$NAME]  $*${NORMAL}"
}


# Output formatted informational messages.
info () {
    env echo -e "${BOLD}INFO   [$NAME]  $*${NORMAL}"
}


# Run a command, logging its output. If an error is encountered, output an error message and exit.
# Args:
#   $@: Command and arguments to be run.
run () {
    info "Running: $* (in $(pwd))."
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
        # Verify that the local path is a directory.
        test -d "$2" \
            || error "Cannot clone $1 to $2; the latter exists and is not a directory."
        # Check whether the directory is empty. If so, just clone.
        if [ "$(stat --format='%h' "$2")" -eq 2 ]; then
            run git clone "$1" "$2"
            return
        fi

        # Attempt to simulate 'git clone' in a non-destructive way. To do this, verify or satisfy
        # the following assertions, and fail otherwise.
        # * The local path is a git repo, in a clean state with no untracked files.
        # * The git repo has a remote named 'origin' matching the given URL.
        # * The git repo has a branch that tracks the origin branch referred to by origin/HEAD.
        # Checkout and update (fetch and merge) the appropriate local branch.

        info "Attempting to simulate 'git clone $1' into existing git repo at $2."
        current_dir="$(pwd)"
        cd "$2"
        # Verify that the local path is a git repository.
        git rev-parse --is-inside-work-tree >/dev/null 2>&1 \
            || error "Cannot clone $1 to $2; existing directory is not a git repo."
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
        # Fetch heads and tags from origin.
        run git fetch origin
        run git remote set-head origin --auto
        # Determine the default remote branch i.e. resolve origin/HEAD. Typically 'master'.
        origin_head="$(git symbolic-ref --quiet refs/remotes/origin/HEAD 2>/dev/null)" \
            || error "Cannot clone $1 to $2; unable to resolve origin/HEAD for existing git repo."
        origin_head="${origin_head#refs/remotes/origin/}"
        # Identify local branches that track origin/HEAD.
        tracking_heads="$(git for-each-ref --format='%(refname) %(upstream)' refs/heads 2>/dev/null \
                            | (grep " refs/remotes/origin/$origin_head" || true) \
                            | cut -d' ' -f1 \
                            | sed -e 's#refs/heads/##')"
        # Checkout a local branch that tracks origin/HEAD. If needed, create the former.
        if [ -z "$tracking_heads" ]; then
            # No local branch tracks origin/HEAD. Verify that no local branch has the same name as
            # origin/HEAD, and then create such a branch to track the latter.
            git show-ref --verify --quiet "refs/heads/$origin_head" 1>/dev/null 2>&1 \
                && error "Cannot clone $1 to $2; existing git repo has a $origin_head branch" \
                    "that does not track origin/$origin_head (the current origin/HEAD)."
            tracking_head="$origin_head"
            run git checkout -b "$tracking_head" --track "origin/$origin_head"
        else
            # One or more local branches track origin/HEAD. If one of these branches has the same
            # name as origin/HEAD, use it. Otherwise, select one arbitrarily.
            if echo "$tracking_heads" | grep -q "^${origin_head}$"; then
                tracking_head="${origin_head}"
            else
                tracking_head="$(echo "$tracking_heads" | head -n1)"
            fi
            current_head="$(git symbolic-ref -q --short HEAD 2>/dev/null)"
            test "$current_head" = "$tracking_head" \
                || run git checkout "$tracking_head"
        fi
        # Update to origin/HEAD, only if the update is a fast-forward i.e. no merge is required.
        run git merge --ff-only "origin/$origin_head" \
            || error "Cannot clone $1 to $2; $tracking_head branch of existing git repo tracks" \
                "origin/$origin_head but has diverged from it."
        cd "$current_dir"
    fi
}


# Create a symlink. Fail if the target exists and is not a symlink.
# Args:
#   $1: Path to which the link points.
#   $2: Link itself.
symlink_safely () {
    # If a non-symlink exists, abort with a warning.
    test -e "$2" && test ! -h "$2" \
        && error "Cannot create symlink $2 -> $1; the former exists and is not a symlink."
    run ln -sf "$1" "$2"
}


# ----------  main  ----------------------------------------------------------

init_logger

# Install freshrc.
clone_safely "$DOTFILES_URL" "$DOTFILES_CLONE"
symlink_safely "$FRESHRC_TARGET" "$FRESHRC_SYMLINK"

# Run fresh.
clone_safely "$FRESH_URL" "$FRESH_CLONE"
run "$FRESH_CLONE/bin/fresh"
