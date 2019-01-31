# Keychain

## Config
* Do not lock when screensaver starts

## Firefox integration
Use the Keychain Services Integration extension. Configure Firefox > Security >
Passwords: verify "Passwords are being managed by ...", check 'Remember
passwords', uncheck 'Use a master password'.

## OpenSSH integration
To have
    brew install openssl && brew link openssl
    brew install openssh --with-brewed-openssl --with-keychain-support
Update /System/Library/LaunchAgents/org.openbsd.ssh-agent.plist to use
/usr/local/bin/ssh.
    ssh-keygen -b 4096 -t rsa -C foo@bar
    security add-generic-password \
        -a /Users/shiv/.ssh/id_rsa \
        -s 'SSH: /Users/shiv/.ssh/id_rsa' \
        -w blah \
        -T /usr/bin/ssh -T /usr/bin/ssh-add -T /usr/bin/ssh-agent \
        -T /usr/local/bin/ssh -T /usr/local/bin/ssh-add \
        -T /usr/local/bin/ssh-agent
Update keychain item as needed.
