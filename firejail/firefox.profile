# Firejail profile for Firefox.

# Persistent local customizations.
include firefox.local
include globals.local

# Network sandbox.
net wlan0
netfilter /etc/firejail/firefox.net
netfilter6 /etc/firejail/firefox.net6
dns 1.1.1.1

# Filesystem sandbox.
private-bin firefox
whitelist /usr/share/mozilla
include whitelist-usr-share-common.inc
#private-etc firefox

noblacklist ${HOME}/.mozilla
mkdir ${HOME}/.mozilla
whitelist ${HOME}/.mozilla
#read-only .../user.js

# Other restrictions.
#memory-deny-write-execute
noexec ${HOME}
nice 20
#rlimit-as 1000000000
ipc-namespace
no3d

include firefox-common.profile
