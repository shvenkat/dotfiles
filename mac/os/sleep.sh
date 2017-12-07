# Disable hibernate. Speeds up sleep (suspend-to-RAM).
sudo pmset -a \
    hibernatemode 0 \
    standby 0 \
    autopoweroff 0

# Set various power management settings.
sudo pmset -a \
    displaysleep 10 \
    sleep 15 \         # Sleep after 15 minutes of idling.
    womp 0 \           # Do not wake on network activity.
    ring 0 \           # Do not wake on modem activity.
    powernap 0 \
    lidwake 0 \        # Do not wake on opening the lid.
    acwake 0 \
    lessbright 0 \
    halfdim 0 \
    ttyskeepawake 0
