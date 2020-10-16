# Firejail profile for emacs (text interface).
# Allowed: read/write access to most of HOME, few executables.
# Restricted: Network, devices, storage.
# To install/update plugins, use the firejail options:
# --net=<interface>  # Allow network access.
# --read-only=${HOME}
# --read-only=${HOME}/.dotfiles
# --read-write=${HOME}/.emacs.d
# --ignore 'private-bin'  # Allow access to curl, tar, gzip, etc.

# Persistent local customizations.
include emacs.local
include globals.local

# Default file permissions.
noblacklist ${HOME}/.emacs
noblacklist ${HOME}/.emacs.d
include allow-common-devel.inc
include disable-common.inc
include disable-passwdmgr.inc
include disable-programs.inc

# Custom file permissions.
noexec ${HOME}
read-write ${HOME}/.dotfiles
read-only ${HOME}/.emacs.d
read-write ${HOME}/.emacs.d/recentf

# Restrict access to system files and storage.
disable-mnt
private-bin emacs*,shellcheck,python3*,bash,ls,df

whitelist /dev/null
whitelist /dev/full
whitelist /dev/zero
whitelist /dev/tty
whitelist /dev/tty1
whitelist /dev/tty2
whitelist /dev/tty3
whitelist /dev/tty4
whitelist /dev/tty5
whitelist /dev/tty6
whitelist /dev/pts
whitelist /dev/ptmx
whitelist /dev/random
whitelist /dev/urandom
whitelist /dev/log
whitelist /dev/shm

whitelist /etc/alternatives
whitelist /etc/ca-certificates.conf
whitelist /etc/ca-certificates
whitelist /etc/crypto-policies
whitelist /etc/fonts
whitelist /etc/ld.so.cache
whitelist /etc/mime.types
whitelist /etc/pango
whitelist /etc/passwd
whitelist /etc/pki
whitelist /etc/selinux
whitelist /etc/ssl
whitelist /etc/terminfo

# Limit privileges, system resources and isolate.
apparmor
caps.drop all
memory-deny-write-execute
nonewprivs
noroot
protocol unix
seccomp
x11 none

rlimit-as 100000000

ipc-namespace
no3d
noautopulse
nodbus
nodvd
nogroups
nosound
notv
nou2f
novideo
shell none

# Network sandbox.
net none
netfilter /etc/firejail/firefox.net
netfilter6 /etc/firejail/firefox.net6
dns 1.1.1.1

# private-cache
