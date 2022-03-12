# ClamAV

## On MacOS

First time, and if needed, after each major OS upgrade:

    sudo mkdir /var/log/clamav /var/clamav
    sudo chown clamav:wheel /var/log/clamav /var/clamav
    cp freshclam.conf /usr/local/etc/clamav/freshclam.conf
    cp clamd.conf /usr/local/etc/clamav/clamd.conf

On each reboot:

    sudo freshclam --daemon
    # sudo clamd  # Uses over 1G of RAM. Use clamscan instead of clamdscan below.
    sudo less /var/log/clamav/freshclam.log
    sudo less /var/log/clamav/clamd.log

To scan files:

    clamdscan --fdpass --multiscan ~/Downloads/reading
