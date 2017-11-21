# Firefox
Plugins: AdblockPlus

# Homebrew
1. Install Xcode command line tools
    xcode-select --install
2. Install homebrew
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew update
    brew doctor
    brew tap caskroom/cask homebrew/dupes ...
    brew install brew-cask
    brew [cask] install ...
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
* Build: cmake, ecj, gcc, gdbm, haskell-stack
* Dev: [cscope], ack, ctags, neovim, the_silver_searcher, tmux, valgrind, vim
    brew rm vim macvim python python3
    brew install vim --with-client-server
* FUSE: osxfuse, sshfs, macfusion
* OS: clamav, coreutils, git, gnupg2, java, libcaca, luajit, mercurial, node,
  openssh, openssh, tunnelblick, xquartz
* Other: flux, screenbrightness
* Utilities: bash-completion, colordiff, grep, htop-osx, jq, less, lesspipe,
    pigz, postgresql, rsync, source-highlight, watch, wget
* VM: awscli, docker, docker-machine, vagrant
    brew install vagrant awscli
    vagrant install vagrant-aws
    vagrant box add dummy https://github.com/mitchellh/vagrant-aws/raw/master/dummy.box
* Work: evernote, google-drive, igv, mendeley-desktop, r, samtools, sqlite
* Science: blast

# Homebrew Setup

* Create a separate (non-root) user account for homebrew installs.

    I don't trust brew install scripts to run with root privileges; but I also
    don't want brew installed files to be modifiable by processes running under
    my regular user account. Hence a separate account for homebrew-ing.

* Run the homebrew install script.

        ./brew_install_all.sh
