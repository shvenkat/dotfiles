# ClamAV

## On MacOS

sudo mkdir /var/log/clamav /var/run/clamav
sudo chown clamav:wheel /var/log/clamav /var/run/clamav
cp freshclam.conf /usr/local/etc/clamav/freshclam.conf
cp clamd.conf /usr/local/etc/clamav/clamd.conf
sudo freshclam --daemon
sudo clamd
clamdscan --fdpass --multiscan ~/Downloads/reading
sudo less /var/log/clamav/freshclam.log
sudo less /var/log/clamav/clamd.log
