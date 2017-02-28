#!/usr/bin/env bash

FRESH_REPO=~/.fresh/source/freshshell/fresh
FRESHRC=~/.freshrc

if [[ -e $FRESHRC ]] && [[ ! -h $FRESHRC ]]; then
    echo "Warning: $FRESHRC exists, aborting."
    exit 1
fi

ln -sf $(dirname $0)/freshrc $FRESHRC \
&& rm -rf $FRESH_REPO \
&& git clone https://github.com/freshshell/fresh $FRESH_REPO \
&& $FRESH_REPO/bin/fresh
