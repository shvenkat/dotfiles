# NOTES

For a discussion on what to place in the various zsh config files, see
https://unix.stackexchange.com/a/71258. In essence:

zshenv: Keep to a bare minimum, as this is read by all zsh processes, including
non-interactive zsh scripts.

zprofile: Place configurations here that may need to be overridden when spawning
interactive subshells. For instance, export environment variables such as PATH
in this file.

zshrc: Define functions, set zsh options, load plugins, etc. here.
