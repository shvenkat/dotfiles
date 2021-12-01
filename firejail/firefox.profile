# Firejail profile for Firefox.
# Allowed: network.
# Restricted: read access to most of HOME, executables, devices, storage.

# Persistent local customizations.
include firefox.local
include globals.local

# Network sandbox.
net wlan0
netfilter /etc/firejail/firefox.net
netfilter6 /etc/firejail/firefox.net6
dns 1.1.1.1

# Filesystem sandbox.
disable-mnt
private-bin firefox
private-dev
private-etc alternatives,asound.conf,ca-certificates.conf,crypto-policies,dconf,fonts,gtk-2.0,gtk-3.0,hostname,hosts,ld.so.cache,machine-id,mime.types,nsswitch.conf,pango,pki,pulse,resolv.conf,selinux,ssl,xdg
private-tmp
whitelist /usr/share/mozilla
include whitelist-usr-share-common.inc

# Custom file permissions.
blacklist /opt
blacklist /run/blkid
blacklist /run/ifstate*
blacklist /run/openrc
blacklist /run/udev
blacklist /srv
blacklist /sys
noexec ${HOME}
whitelist ${DOWNLOADS}
# noblacklist ${HOME}/.mozilla
# mkdir ${HOME}/.mozilla
# whitelist ${HOME}/.mozilla
noblacklist ${HOME}/.local/share/pki
mkdir ${HOME}/.local/share/pki
whitelist ${HOME}/.local/share/pki
noblacklist ${HOME}/.pki
mkdir ${HOME}/.pki
whitelist ${HOME}/.pki

# Limit privileges, system resources and isolate.
apparmor
caps.drop all
# Firefox does not display a window on starting if memory-deny-write-execute is enabled.
#memory-deny-write-execute
nonewprivs
noroot
protocol unix,inet,inet6,netlink
# Firefox uses chroot to restrict itself.
seccomp !chroot
# TODO: Use a sandboxed X server. For now, use a separate non-root X server.
#x11 none

# Firefox exits if a memory limit is enforced.
#rlimit-as 1000000000
nice 20

ipc-namespace
no3d
nodbus
nodvd
nogroups
notv
shell none

include firefox-common.profile
