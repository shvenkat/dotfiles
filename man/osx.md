# Firefox
Plugins: AdblockPlus

# Homebrew
1. Install Xcode command line tools
    xcode-select --install
2. Install homebrew
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew update
    brew doctor
    brew install brew-cask
3. Tap useful repos: caskroom/cask, homebrew/dupes, homebrew/fuse,
   homebrew/science, neovim/neovim

# Terminal
1. Install git
2. Clone and deploy .rc repo
3. Import and enable Terminal config from .rc repo
4. Configure Finder to show all files
    defaults write com.apple.finder AppleShowAllFiles TRUE

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

# Fonts
* Droid Sans Mono [Dotted/Slashed] for Powerline
* Inconsolata[-dz/-g] for Powerline
* DejaVu Sans Mono for Powerline

# Misc
* Build: cmake, ecj, gcc, gdbm
* Dev: [cscope], ack, ctags, neovim, the_silver_searcher, tmux, vim
    brew rm vim macvim python python3
    brew install vim --with-client-server
* FUSE: osxfuse, sshfs, macfusion
* OS: git, gnupg2, java, mercurial, openssh, openssh, tunnelblick, xquartz
* Other: flux, screenbrightness
* Utilities: bash-completion, colordiff, grep, less, lesspipe, rsync,
  source-highlight, wget
* VM: awscli, vagrant
    brew install vagrant awscli
    vagrant install vagrant-aws
    vagrant box add dummy https://github.com/mitchellh/vagrant-aws/raw/master/dummy.box
* Work: evernote, google-drive, igv, mendeley-desktop, r, samtools, sqlite

