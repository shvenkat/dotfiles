#!/bin/bash

set -e -u -o pipefail

# Disable boot sound effects.
sudo nvram SystemAudioVolume=" "

# Warn if a lock screen message has not been set. Setting a message may help in
# recovering a lost machine.
if ! nvram -p | grep -q good-samaritan-message; then
    warn "Lock screen message has not been set. You can set one in" \
        "System Preferences > Security & Privacy > General > Set Lock Message" \
        "to provide contact info to the person who finds your lost machine."
fi
