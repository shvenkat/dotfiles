# ----------  Hibernate / Suspend-to-Disk / SafeSleep  -----------------------

# Disable hibernate. Speeds up sleep (suspend-to-RAM) and wake.
sudo pmset -a hibernatemode 0
sudo pmset -a standby 0
sudo pmset -a autopoweroff 0

# Remove the hibernate image file to save disk space, and prevent it from being created.
sudo rm /private/var/vm/sleepimage
sudo touch /private/var/vm/sleepimage
sudo chflags uchg /private/var/vm/sleepimage


# ----------  Sleep / Suspend-to-RAM  ----------------------------------------

sudo pmset -a displaysleep 10
sudo pmset -a sleep 30
sudo pmset -a womp 0
sudo pmset -a ring 0
sudo pmset -a powernap 0
sudo pmset -a lidwake 0
sudo pmset -a acwake 0
sudo pmset -a lessbright 0
sudo pmset -a halfdim 0
sudo pmset -a ttyskeepawake 0
