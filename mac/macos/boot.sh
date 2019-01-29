#!/bin/bash

set -e -u -o pipefail

# Disable boot sound effects.
sudo nvram SystemAudioVolume=' '

# Warn if a lock screen message has not been set. Setting a message may help in
# recovering a lost machine.
if ! nvram -p | grep -q good-samaritan-message; then
    echo -e '\033[31mWARNING: Lock screen message has not been set\033[0m.' \
        'You can set one in' \
        'System Preferences > Security & Privacy > General > Set Lock Message' \
        'to provide contact info to the person who finds your lost machine.'
fi
