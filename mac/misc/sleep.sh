#!/bin/bash

set -e -u -o pipefail

# Disable hibernate. Speeds up sleep (suspend-to-RAM).
sudo pmset -a \
    hibernatemode 0 \
    standby 0 \
    autopoweroff 0

# Set various power management settings.
#   sleep: Sleep after 15 minutes of idling.
#   womp:  Disable Wake-on-LAN.
#   ring:  Disable "wake-on-modem".
#   lidwake: Opening the lid does not awaken the laptop.
sudo pmset -a \
    displaysleep 10 \
    sleep 15 \
    womp 0 \
    ring 0 \
    powernap 0 \
    lidwake 0 \
    acwake 0 \
    lessbright 0 \
    halfdim 0 \
    ttyskeepawake 0
