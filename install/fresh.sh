#!/bin/sh

set -e -u -o pipefail

REPO_SRC="https://github.com/freshshell/fresh"
REPO_DST="${HOME}/.fresh/source/freshshell/fresh"
RC_SRC="$(cd "$(dirname -- "$0")"; pwd)/freshrc"
RC_DST="${HOME}/.freshrc"

# If a non-symlink exists at the target location, abort with a warning.
if [ -e "$RC_DST" ] && [ ! -h "$RC_DST" ]; then
    echo "Warning: $RC_DST exists, aborting."
    exit 1
fi

# Clone fresh, symlink freshrc and run fresh.
ln -sf "$RC_SRC" "$RC_DST" \
&& rm -rf "$REPO_DST" \
&& git clone "$REPO_SRC" "$REPO_DST" \
&& "$REPO_DST/bin/fresh"
