#!/bin/bash

tap () {
    cat $1 | xargs -L1 brew tap
}

install () {
    cat $1 | xargs -L1 brew install
}

install_casks () {
    cat $1 | xargs -L1 brew cask install
}

if which brew >/dev/null && grep -qi homebrew $(brew --version); then
    this_dir=$(dirname $0)
    tap $this_dir/homebrew_taps.txt
    install $this_dir/homebrew_leaves.txt
    install_casks $this_dir/homebrew_casks.txt
fi
