#!/usr/bin/env bash

BORKRC=~/.borkrc

if [[ -e $BORKRC ]] && [[ ! -h $BORKRC ]]; then
    echo "Warning: $BORKRC exists, aborting."
    exit 1
fi

ln -sf $(dirname $0)/borkrc $BORKRC \
&& bork satisfy $BORKRC
