#!/bin/bash

if which brew > /dev/null && brew --version | grep -qi homebrew; then
    this_dir=$(dirname $0)
    brew tap > $this_dir/homebrew_taps.txt
    brew leaves > $this_dir/homebrew_leaves.txt
    brew cask list > $this_dir/homebrew_casks.txt
fi
