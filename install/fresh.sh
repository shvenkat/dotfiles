#!/bin/sh

set -e -u -o pipefail

DOTFILES_SRC="https://github.com/shvenkat/dotfiles"
DOTFILES_CLONE="${HOME}/.fresh/source/shvenkat/dotfiles"
FRESH_SRC="https://github.com/freshshell/fresh"
FRESH_CLONE="${HOME}/.fresh/source/freshshell/fresh"
FRESHRC_SRC="${DOTFILES_CLONE}/install/freshrc"
FRESHRC_DST="${HOME}/.freshrc"
NAME="$(basename "$0")"


# Write to stderr.
stderr () {
    echo "$@" 1>&2
}


# Write formatted error messages to stderr.
error () {
    stderr "ERROR	[${NAME}]$*."
    exit 1
}


# Write formatted error messages to stderr.
info () {
    echo "[${NAME}]	INFO	$*."
}


# Clone a git repo, or update (pull) an existing one. Fail if the destination exists and cannot be
# updated to match the result of a clone operation.
# Args:
#   $1: Remote URL, e.g. https://github.com/foo/bar.
#   $2: Local path.
clone_safely () {
    if [ ! -e "$2" ]; then
        echo
        git clone "$1" "$2"
    else
        # Verify that the local path is a directory.
        test -d "$2" \
            || error "Cannot clone $1; incompatible file type exists at $2."
        current_dir="$(pwd)"
        cd "$2"
        # Temporarily suppress output while checking whether the existing directory.
        exec

        # Verify that the local path is a git repo, in a clean state with no untracked files, has
        # an origin matching the given remote URL and a master branch tracking the remote master. If
        # so, update it. Otherwise, complain and exit.

        # Verify that the local path is a git repository.
        git rev-parse --is-inside-work-tree >/dev/null 2>&1 \
            || error "Cannot clone $1; existing directory at $2 is not a git repository."
        # Verify that the working tree is clean regarding tracked files.
        git diff-index --quiet HEAD >/dev/null 2>&1 \
            || error "Cannot clone $1; existing git repo at $2 is not clean."
        # Verify that there are no untracked files. These could conflict with a pull.
        test -z "$(git ls-files --others --exclude-standard 2>/dev/null)" \
            || error "Cannot clone $1; existing git repo at $2 has untracked files."
        # Verify that a remote named 'origin' exists and has the given URL.
        if (git remote | grep -q '^origin$') >/dev/null 2>&1; then
            # Verify that the URL of the remote 'origin' matches the given one.
            test "$1" = "$(git ls-remote --get-url origin 2>/dev/null)" \
                || error "Cannot clone $1; existing git repo at $2 has a different 'origin' remote."
        else
            # Add an 'origin' remote.
            git remote add origin "$1"
        fi
        # Determine the default remote branch i.e. resolve origin/HEAD. Typically 'master'.
        origin_head="$(git ls-remote --symref origin HEAD | grep -o 'refs/heads/[^[:space:]]\+')"
        origin_head="${origin_head#refs/heads/}"
        # Ensure that a local tracking branch exists for the default remote branch. Find its name if
        # it exists or create one.
        tracking_heads="$(git for-each-ref --format='%(refname) %(upstream)' refs/heads \
                            | (grep " refs/remotes/origin/$origin_head" || true) \
                            | cut -d' ' -f1 \
                            | sed -e 's#refs/heads/##')"
        if [ -z "$tracking_heads" ]; then
            # Verify the absence of a local branch with the same name as origin/HEAD.
            git show-ref --verify --quiet "refs/heads/$origin_head" \
                || error "Cannot clone $1; branch $origin_head in the existing git repo at $2" \
                    "does not track origin/$origin_head (the current origin/HEAD)."
            # Create a local branch that tracks origin/HEAD.
            git checkout -b "$origin_head" --track "origin/$origin_head"
        else
            # If there is a single matching local branch, use it. If there are multiple, select one
            # arbitrarily.
            local_head="$(echo "$tracking_heads" | head -n1)"
            git checkout "$local_head"
        fi
        # Fetch and merge.
        git pull

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
}


# ----------  main  ----------------------------------------------------------

# Install fresh config.
clone_safely "$DOTFILES_SRC" "$DOTFILES_CLONE"
symlink_safely "$FRESHRC_SRC" "$FRESHRC_DST"

# Run fresh.
clone_safely "$FRESH_SRC" "$FRESH_CLONE"
"$FRESH_CLONE/bin/fresh"
